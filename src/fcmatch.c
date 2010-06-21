/*
 * fontconfig/src/fcmatch.c
 *
 * Copyright © 2000 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Keith Packard not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Keith Packard makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE AUTHOR(S) DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */
/*
  Copyright © 2010 Barry Schwartz

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

#include "fcint.h"
#include <string.h>
#include <ctype.h>
#include <stdio.h>

static double
FcCompareNumber (FcValue *value1, FcValue *value2)
{
    double  v1, v2, v;

    switch (value1->type) {
    case FcTypeInteger:
	v1 = (double) value1->u.i;
	break;
    case FcTypeDouble:
	v1 = value1->u.d;
	break;
    default:
	return -1.0;
    }
    switch (value2->type) {
    case FcTypeInteger:
	v2 = (double) value2->u.i;
	break;
    case FcTypeDouble:
	v2 = value2->u.d;
	break;
    default:
	return -1.0;
    }
    v = v2 - v1;
    if (v < 0)
	v = -v;
    return v;
}

static double
FcCompareString (FcValue *v1, FcValue *v2)
{
    return (double) FcStrCmpIgnoreCase (FcValueString(v1), FcValueString(v2)) != 0;
}

static double
FcCompareFamily (FcValue *v1, FcValue *v2)
{
    /* rely on the guarantee in FcPatternObjectAddWithBinding that
     * families are always FcTypeString. */
    const FcChar8* v1_string = FcValueString(v1);
    const FcChar8* v2_string = FcValueString(v2);

    if (FcToLower(*v1_string) != FcToLower(*v2_string) &&
	*v1_string != ' ' && *v2_string != ' ')
       return 1.0;

    return (double) FcStrCmpIgnoreBlanksAndCase (v1_string, v2_string) != 0;
}

static double
FcCompareLang (FcValue *v1, FcValue *v2)
{
    FcLangResult    result;
    FcValue value1 = FcValueCanonicalize(v1), value2 = FcValueCanonicalize(v2);

    switch (value1.type) {
    case FcTypeLangSet:
	switch (value2.type) {
	case FcTypeLangSet:
	    result = FcLangSetCompare (value1.u.l, value2.u.l);
	    break;
	case FcTypeString:
	    result = FcLangSetHasLang (value1.u.l,
				       value2.u.s);
	    break;
	default:
	    return -1.0;
	}
	break;
    case FcTypeString:
	switch (value2.type) {
	case FcTypeLangSet:
	    result = FcLangSetHasLang (value2.u.l, value1.u.s);
	    break;
	case FcTypeString:
	    result = FcLangCompare (value1.u.s,
				    value2.u.s);
	    break;
	default:
	    return -1.0;
	}
	break;
    default:
	return -1.0;
    }
    switch (result) {
    case FcLangEqual:
	return 0;
    case FcLangDifferentCountry:
	return 1;
    case FcLangDifferentLang:
    default:
	return 2;
    }
}

static double
FcCompareBool (FcValue *v1, FcValue *v2)
{
    if (v2->type != FcTypeBool || v1->type != FcTypeBool)
	return -1.0;
    return (double) v2->u.b != v1->u.b;
}

static double
FcCompareCharSet (FcValue *v1, FcValue *v2)
{
    return (double) FcCharSetSubtractCount (FcValueCharSet(v1), FcValueCharSet(v2));
}

static double
FcCompareSize (FcValue *value1, FcValue *value2)
{
    double  v1, v2, v;

    switch (value1->type) {
    case FcTypeInteger:
	v1 = value1->u.i;
	break;
    case FcTypeDouble:
	v1 = value1->u.d;
	break;
    default:
	return -1;
    }
    switch (value2->type) {
    case FcTypeInteger:
	v2 = value2->u.i;
	break;
    case FcTypeDouble:
	v2 = value2->u.d;
	break;
    default:
	return -1;
    }
    if (v2 == 0)
	return 0;
    v = v2 - v1;
    if (v < 0)
	v = -v;
    return v;
}

typedef struct _FcMatcher {
    FcObject	    object;
    double	    (*compare) (FcValue *value1, FcValue *value2);
} FcMatcher;

static FcObject default_priority_order [] = {
    FC_FOUNDRY_OBJECT,
    FC_CHARSET_OBJECT,
    FC_FAMILY_OBJECT,           /* strong */
    FC_LANG_OBJECT,
    FC_FAMILY_OBJECT,           /* weak */
    FC_SPACING_OBJECT,
    FC_PIXEL_SIZE_OBJECT,
    FC_STYLE_OBJECT,
    FC_SLANT_OBJECT,
    FC_WEIGHT_OBJECT,
    FC_WIDTH_OBJECT,
    FC_DECORATIVE_OBJECT,
    FC_ANTIALIAS_OBJECT,
    FC_RASTERIZER_OBJECT,
    FC_OUTLINE_OBJECT,
    FC_FONTVERSION_OBJECT
};

static FcMatcher _FcMatchers [] = {
    { FC_FOUNDRY_OBJECT,     FcCompareString },
    { FC_CHARSET_OBJECT,     FcCompareCharSet },
    { FC_FAMILY_OBJECT,      FcCompareFamily },
    { FC_LANG_OBJECT,        FcCompareLang },
    { FC_SPACING_OBJECT,     FcCompareNumber },
    { FC_PIXEL_SIZE_OBJECT,  FcCompareSize },
    { FC_STYLE_OBJECT,       FcCompareString },
    { FC_SLANT_OBJECT,       FcCompareNumber },
    { FC_WEIGHT_OBJECT,      FcCompareNumber },
    { FC_WIDTH_OBJECT,       FcCompareNumber },
    { FC_DECORATIVE_OBJECT,  FcCompareBool },
    { FC_ANTIALIAS_OBJECT,   FcCompareBool },
    { FC_RASTERIZER_OBJECT,  FcCompareString },
    { FC_OUTLINE_OBJECT,     FcCompareBool },
    { FC_FONTVERSION_OBJECT, FcCompareNumber  }
};

#define NUM_MATCHERS             (sizeof _FcMatchers / sizeof _FcMatchers[0])
#define DEFAULT_NUM_MATCH_VALUES (sizeof default_priority_order / sizeof default_priority_order[0])
#define NUM_MATCH_VALUES     (2 * NUM_MATCHERS)

static int matcher_index [FC_MAX_BASE_OBJECT + 1];

void
FcInitMatchers (void)
{
    int i;

    for (i = 0;  i <= FC_MAX_BASE_OBJECT;  i++)
        matcher_index[i] = -1;

    for (i = 0;  i < NUM_MATCHERS;  i++)
        matcher_index[_FcMatchers[i].object] = i;
}

void
FcInitPriorities (FcConfig *config, FcObject *priority_order, int n)
{
    int i;
    int j;

    if (priority_order == NULL) {
        priority_order = default_priority_order;
        n = DEFAULT_NUM_MATCH_VALUES;
    }

    for (j = 0;  j <= FC_MAX_BASE_OBJECT;  j++) {
        config->priorities.strong[j] = -1;
        config->priorities.weak[j] = -1;
    }

    for (i = 0;  i < n;  i++) {
        j = matcher_index[priority_order[i]];
        if (config->priorities.strong[j] < 0)
            config->priorities.strong[j] = i;
        else
            config->priorities.weak[j] = i;
    }

    for (i = 0;  i < n;  i++) {
        j = matcher_index[priority_order[i]];
        if (config->priorities.weak[j] < 0)
            config->priorities.weak[j] = config->priorities.strong[j];
    }
}

static const FcMatcher *
FcObjectToMatcher (FcObject object)
{
    int i;

    i = matcher_index[object];
    return (0 < i) ? &_FcMatchers[i] : NULL;
}

static FcBool
FcCompareValueList (FcConfig	 *config,
                    FcObject	 object,
                    FcValueListPtr v1orig,	/* pattern */
                    FcValueListPtr v2orig,	/* target */
                    FcValue	*bestValue,
                    double	*value,
                    FcResult	*result)
{
    FcValueListPtr  v1, v2;
    double    	    v, best, bestStrong, bestWeak;
    int		    j;
    const FcMatcher *match = FcObjectToMatcher(object);

    if (!match)
    {
	if (bestValue)
	    *bestValue = FcValueCanonicalize(&v2orig->value);
	return FcTrue;
    }

    best = 1e99;
    bestStrong = 1e99;
    bestWeak = 1e99;
    j = 1;
    for (v1 = v1orig; v1; v1 = FcValueListNext(v1))
    {
	for (v2 = v2orig; v2; v2 = FcValueListNext(v2))
	{
	    v = (match->compare) (&v1->value, &v2->value);
	    if (v < 0)
	    {
		*result = FcResultTypeMismatch;
		return FcFalse;
	    }
	    v = v * 1000 + j;
	    if (v < best)
	    {
		if (bestValue)
		    *bestValue = FcValueCanonicalize(&v2->value);
		best = v;
	    }
	    if (v1->binding == FcValueBindingStrong)
	    {
		if (v < bestStrong)
		    bestStrong = v;
	    }
	    else
	    {
		if (v < bestWeak)
		    bestWeak = v;
	    }
	}
	j++;
    }
    if (FcDebug () & FC_DBG_MATCHV)
    {
	printf (" %s: %g ", FcObjectName (object), best);
	FcValueListPrint (v1orig);
	printf (", ");
	FcValueListPrint (v2orig);
	printf ("\n");
    }
    if (value)
    {
        j = matcher_index[object];
        int weak = config->priorities.weak[j];
        int strong = config->priorities.strong[j];
        if (weak == strong) {
            value[strong] += best;
        } else {
            value[weak] += bestWeak;
            value[strong] += bestStrong;
        }
    }
    return FcTrue;
}

/*
 * Return a value indicating the distance between the two lists of
 * values
 */

static FcBool
FcCompare (FcConfig	*config,
           FcPattern	*pat,
           FcPattern	*fnt,
           double	*value,
           FcResult	*result)
{
    int		    i, i1, i2;

    for (i = 0; i < NUM_MATCH_VALUES; i++)
	value[i] = 0.0;

    i1 = 0;
    i2 = 0;
    while (i1 < pat->num && i2 < fnt->num)
    {
	FcPatternElt *elt_i1 = &FcPatternElts(pat)[i1];
	FcPatternElt *elt_i2 = &FcPatternElts(fnt)[i2];

	i = FcObjectCompare(elt_i1->object, elt_i2->object);
	if (i > 0)
	    i2++;
	else if (i < 0)
	    i1++;
	else
	{
	    if (!FcCompareValueList (config, elt_i1->object,
				     FcPatternEltValues(elt_i1),
				     FcPatternEltValues(elt_i2),
				     0, value, result))
		return FcFalse;
	    i1++;
	    i2++;
	}
    }
    return FcTrue;
}

FcPattern *
FcFontRenderPrepare (FcConfig	    *config,
		     FcPattern	    *pat,
		     FcPattern	    *font)
{
    FcPattern	    *new;
    int		    i;
    FcPatternElt    *fe, *pe;
    FcValue	    v;
    FcResult	    result;

    new = FcPatternCreate ();
    if (!new)
	return 0;
    for (i = 0; i < font->num; i++)
    {
	fe = &FcPatternElts(font)[i];
	pe = FcPatternObjectFindElt (pat, fe->object);
	if (pe)
	{
	    if (!FcCompareValueList (config, pe->object, FcPatternEltValues(pe),
				     FcPatternEltValues(fe), &v, 0, &result))
	    {
		FcPatternDestroy (new);
		return 0;
	    }
	}
	else
	    v = FcValueCanonicalize(&FcPatternEltValues (fe)->value);
	FcPatternObjectAdd (new, fe->object, v, FcFalse);
    }
    for (i = 0; i < pat->num; i++)
    {
	pe = &FcPatternElts(pat)[i];
	fe = FcPatternObjectFindElt (font, pe->object);
	if (!fe)
	{
	    v = FcValueCanonicalize(&FcPatternEltValues(pe)->value);
	    FcPatternObjectAdd (new, pe->object, v, FcTrue);
	}
    }

    FcConfigSubstituteWithPat (config, new, pat, FcMatchFont);
    return new;
}

static FcPattern *
FcFontSetMatchInternal (FcConfig    *config,
			FcFontSet   **sets,
			int	    nsets,
			FcPattern   *p,
			FcResult    *result)
{
    double    	    score[NUM_MATCH_VALUES], bestscore[NUM_MATCH_VALUES];
    int		    f;
    FcFontSet	    *s;
    FcPattern	    *best;
    int		    i;
    int		    set;

    for (i = 0; i < NUM_MATCH_VALUES; i++)
	bestscore[i] = 0;
    best = 0;
    if (FcDebug () & FC_DBG_MATCH)
    {
	printf ("Match ");
	FcPatternPrint (p);
    }
    for (set = 0; set < nsets; set++)
    {
	s = sets[set];
	if (!s)
	    continue;
	for (f = 0; f < s->nfont; f++)
	{
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Font %d ", f);
		FcPatternPrint (s->fonts[f]);
	    }
	    if (!FcCompare (config, p, s->fonts[f], score, result))
		return 0;
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Score");
		for (i = 0; i < NUM_MATCH_VALUES; i++)
		{
		    printf (" %g", score[i]);
		}
		printf ("\n");
	    }
	    for (i = 0; i < NUM_MATCH_VALUES; i++)
	    {
		if (best && bestscore[i] < score[i])
		    break;
		if (!best || score[i] < bestscore[i])
		{
		    for (i = 0; i < NUM_MATCH_VALUES; i++)
			bestscore[i] = score[i];
		    best = s->fonts[f];
		    break;
		}
	    }
	}
    }
    if (FcDebug () & FC_DBG_MATCH)
    {
	printf ("Best score");
	for (i = 0; i < NUM_MATCH_VALUES; i++)
	    printf (" %g", bestscore[i]);
	printf ("\n");
	FcPatternPrint (best);
    }
    if (!best)
    {
	*result = FcResultNoMatch;
	return 0;
    }
    return best;
}

FcPattern *
FcFontSetMatch (FcConfig    *config,
		FcFontSet   **sets,
		int	    nsets,
		FcPattern   *p,
		FcResult    *result)
{
    FcPattern	    *best;

    if (!config)
    {
	config = FcConfigGetCurrent ();
	if (!config)
	    return 0;
    }
    best = FcFontSetMatchInternal (config, sets, nsets, p, result);
    if (best)
	return FcFontRenderPrepare (config, p, best);
    else
	return NULL;
}

FcPattern *
FcFontMatch (FcConfig	*config,
	     FcPattern	*p,
	     FcResult	*result)
{
    FcFontSet	*sets[2];
    int		nsets;
    FcPattern   *best;

    if (!config)
    {
	config = FcConfigGetCurrent ();
	if (!config)
	    return 0;
    }
    nsets = 0;
    if (config->fonts[FcSetSystem])
	sets[nsets++] = config->fonts[FcSetSystem];
    if (config->fonts[FcSetApplication])
	sets[nsets++] = config->fonts[FcSetApplication];

    best = FcFontSetMatchInternal (config, sets, nsets, p, result);
    if (best)
	return FcFontRenderPrepare (config, p, best);
    else
	return NULL;
}

typedef struct _FcSortNode {
    FcPattern	*pattern;
    double	score[NUM_MATCH_VALUES];
} FcSortNode;

static int
FcSortCompare (const void *aa, const void *ab)
{
    FcSortNode  *a = *(FcSortNode **) aa;
    FcSortNode  *b = *(FcSortNode **) ab;
    double	*as = &a->score[0];
    double	*bs = &b->score[0];
    double	ad = 0, bd = 0;
    int         i;

    i = NUM_MATCH_VALUES;
    while (i-- && (ad = *as++) == (bd = *bs++))
        ;
    return ad < bd ? -1 : ad > bd ? 1 : 0;
}

static FcBool
FcSortWalk (FcSortNode **n, int nnode, FcFontSet *fs, FcCharSet **csp, FcBool trim)
{
    FcBool ret = FcFalse;
    FcCharSet *cs;

    cs = 0;
    if (trim || csp)
    {
	cs = FcCharSetCreate ();
	if (cs == NULL)
	    goto bail;
    }

    while (nnode--)
    {
	FcSortNode	*node = *n++;
	FcBool		adds_chars = FcFalse;

	/*
	 * Only fetch node charset if we'd need it
	 */
	if (cs)
	{
	    FcCharSet	*ncs;

	    if (FcPatternGetCharSet (node->pattern, FC_CHARSET, 0, &ncs) !=
		FcResultMatch)
	        continue;

	    if (!FcCharSetMerge (cs, ncs, &adds_chars))
		goto bail;
	}

	/*
	 * If this font isn't a subset of the previous fonts,
	 * add it to the list
	 */
	if (!trim || adds_chars)
	{
	    FcPatternReference (node->pattern);
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Add ");
		FcPatternPrint (node->pattern);
	    }
	    if (!FcFontSetAdd (fs, node->pattern))
	    {
		FcPatternDestroy (node->pattern);
		goto bail;
	    }
	}
    }
    if (csp)
    {
	*csp = cs;
	cs = 0;
    }

    ret = FcTrue;

bail:
    if (cs)
	FcCharSetDestroy (cs);

    return ret;
}

void
FcFontSetSortDestroy (FcFontSet *fs)
{
    FcFontSetDestroy (fs);
}

FcFontSet *
FcFontSetSort (FcConfig	    *config,
	       FcFontSet    **sets,
	       int	    nsets,
	       FcPattern    *p,
	       FcBool	    trim,
	       FcCharSet    **csp,
	       FcResult	    *result)
{
    FcFontSet	    *ret;
    FcFontSet	    *s;
    FcSortNode	    *nodes;
    FcSortNode	    **nodeps, **nodep;
    int		    nnodes;
    FcSortNode	    *new;
    int		    set;
    int		    f;
    int		    i;
    int		    nPatternLang;
    FcBool    	    *patternLangSat;
    FcValue	    patternLang;

    if (FcDebug () & FC_DBG_MATCH)
    {
	printf ("Sort ");
	FcPatternPrint (p);
    }
    nnodes = 0;
    for (set = 0; set < nsets; set++)
    {
	s = sets[set];
	if (!s)
	    continue;
	nnodes += s->nfont;
    }
    if (!nnodes)
	goto bail0;

    for (nPatternLang = 0;
	 FcPatternGet (p, FC_LANG, nPatternLang, &patternLang) == FcResultMatch;
	 nPatternLang++)
	;
	
    /* freed below */
    nodes = malloc (nnodes * sizeof (FcSortNode) +
		    nnodes * sizeof (FcSortNode *) +
		    nPatternLang * sizeof (FcBool));
    if (!nodes)
	goto bail0;
    nodeps = (FcSortNode **) (nodes + nnodes);
    patternLangSat = (FcBool *) (nodeps + nnodes);

    new = nodes;
    nodep = nodeps;
    for (set = 0; set < nsets; set++)
    {
	s = sets[set];
	if (!s)
	    continue;
	for (f = 0; f < s->nfont; f++)
	{
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Font %d ", f);
		FcPatternPrint (s->fonts[f]);
	    }
	    new->pattern = s->fonts[f];
	    if (!FcCompare (config, p, new->pattern, new->score, result))
		goto bail1;
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Score");
		for (i = 0; i < NUM_MATCH_VALUES; i++)
		{
		    printf (" %g", new->score[i]);
		}
		printf ("\n");
	    }
	    *nodep = new;
	    new++;
	    nodep++;
	}
    }

    nnodes = new - nodes;

    qsort (nodeps, nnodes, sizeof (FcSortNode *),
	   FcSortCompare);

    for (i = 0; i < nPatternLang; i++)
	patternLangSat[i] = FcFalse;

    for (f = 0; f < nnodes; f++)
    {
	FcBool	satisfies = FcFalse;
	/*
	 * If this node matches any language, go check
	 * which ones and satisfy those entries
	 */
	if (nodeps[f]->score[matcher_index[FC_LANG_OBJECT]] < 200)
	{
	    for (i = 0; i < nPatternLang; i++)
	    {
		FcValue	    nodeLang;
		
		if (!patternLangSat[i] &&
		    FcPatternGet (p, FC_LANG, i, &patternLang) == FcResultMatch &&
		    FcPatternGet (nodeps[f]->pattern, FC_LANG, 0, &nodeLang) == FcResultMatch)
		{
		    double  compare = FcCompareLang (&patternLang, &nodeLang);
		    if (compare >= 0 && compare < 2)
		    {
			if (FcDebug () & FC_DBG_MATCHV)
			{
			    FcChar8 *family;
			    FcChar8 *style;

			    if (FcPatternGetString (nodeps[f]->pattern, FC_FAMILY, 0, &family) == FcResultMatch &&
				FcPatternGetString (nodeps[f]->pattern, FC_STYLE, 0, &style) == FcResultMatch)
				printf ("Font %s:%s matches language %d\n", family, style, i);
			}
			patternLangSat[i] = FcTrue;
			satisfies = FcTrue;
			break;
		    }
		}
	    }
	}
	if (!satisfies)
	    nodeps[f]->score[matcher_index[FC_LANG_OBJECT]] = 10000.0;
    }

    /*
     * Re-sort once the language issues have been settled
     */
    qsort (nodeps, nnodes, sizeof (FcSortNode *),
	   FcSortCompare);

    ret = FcFontSetCreate ();
    if (!ret)
	goto bail1;

    if (!FcSortWalk (nodeps, nnodes, ret, csp, trim))
	goto bail2;

    free (nodes);

    if (FcDebug() & FC_DBG_MATCH)
    {
	printf ("First font ");
	FcPatternPrint (ret->fonts[0]);
    }
    return ret;

bail2:
    FcFontSetDestroy (ret);
bail1:
    free (nodes);
bail0:
    return 0;
}

FcFontSet *
FcFontSort (FcConfig	*config,
	    FcPattern	*p,
	    FcBool	trim,
	    FcCharSet	**csp,
	    FcResult	*result)
{
    FcFontSet	*sets[2];
    int		nsets;

    if (!config)
    {
	config = FcConfigGetCurrent ();
	if (!config)
	    return 0;
    }
    nsets = 0;
    if (config->fonts[FcSetSystem])
	sets[nsets++] = config->fonts[FcSetSystem];
    if (config->fonts[FcSetApplication])
	sets[nsets++] = config->fonts[FcSetApplication];
    return FcFontSetSort (config, sets, nsets, p, trim, csp, result);
}
#define __fcmatch__
#include "fcaliastail.h"
#undef __fcmatch__
