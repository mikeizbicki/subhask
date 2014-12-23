#include <math.h>
#include <xmmintrin.h>

float distance_l2_float(float *p1, float *p2, int len)
{
    float ret=0;
    int i=0;
    for (i=0; i<len; i++) {
        ret+=pow((p1[i]-p2[i]),2);
    }
    return sqrt(ret);
}

float isFartherThan_l2_float(float *p1, float *p2, int len, float dist)
{
    float ret=0;
    float dist2=dist*dist;
    int i=0;
    for (i=0; i<len; i++) {
        ret+=pow((p1[i]-p2[i]),2);
        if (ret > dist2) return NAN;
    }
    return sqrt(ret);
}

double distance_l2_double(double *p1, double *p2, int len)
{
    double ret=0;
    int i=0;
    for (i=0; i<len; i++) {
        ret+=pow((p1[i]-p2[i]),2);
    }
    return sqrt(ret);
}

double isFartherThan_l2_double(double *p1, double *p2, int len, double dist)
{
    double ret=0;
    double dist2=dist*dist;
    int i=0;
    for (i=0; i<len; i++) {
        ret+=pow((p1[i]-p2[i]),2);
        if (ret > dist2) return NAN;
    }
    return sqrt(ret);
}

/******************************************************************************/
/* __m128 */

float distance_l2_m128(__m128 *p1, __m128 *p2, int len)
{
    float ret=0;
    __m128 sum={0,0,0,0};

    int i=0;
    for (i=0; i<len/4; i++) {
        __m128 diff;
        diff = _mm_sub_ps(p1[i],p2[i]);
        sum = _mm_add_ps(sum,_mm_mul_ps(diff,diff));
    }

    ret = sum[0] + sum[1] + sum[2] + sum[3];

    for (i*=4; i<len; i++) {
        ret += pow(((float*)p1)[i]-((float*)p2)[i],2);
    }

    return sqrt(ret);
}

float isFartherThan_l2_m128(__m128 *p1, __m128 *p2, int len, float dist)
{
    float ret=0;
    float dist2=dist*dist;
    __m128 sum={0,0,0,0};

    int i=0;
    for (i=0; i<len/4; i++) {
        __m128 diff;
        diff = _mm_sub_ps(p1[i],p2[i]);
        sum = _mm_add_ps(sum,_mm_mul_ps(diff,diff));

        // moving information out of the simd registers is expensive,
        // so we don't do it on every iteration
        if (i%4==0 && sum[0] > dist2/4) {
            if (sum[0]+sum[1]+sum[2]+sum[3] > dist2) {
                return NAN;
            }
        }
    }

    ret = sum[0] + sum[1] + sum[2] + sum[3];

    for (i*=4; i<len; i++) {
        ret += pow(((float*)p1)[i]-((float*)p2)[i],2);
    }
    if (ret > dist2) {
        return NAN;
    }

    return sqrt(ret);
}

float isFartherThan_l2_m128_nocheck(__m128 *p1, __m128 *p2, int len, float dist)
{
    float ret=0;
    float dist2=dist*dist;
    __m128 sum={0,0,0,0};

    int i=0;
    for (i=0; i<len/4; i++) {
        __m128 diff;
        diff = _mm_sub_ps(p1[i],p2[i]);
        sum = _mm_add_ps(sum,_mm_mul_ps(diff,diff));
    }

    ret = sum[0] + sum[1] + sum[2] + sum[3];

    for (i*=4; i<len; i++) {
        ret += pow(((float*)p1)[i]-((float*)p2)[i],2);
    }

    return sqrt(ret);
}

/******************************************************************************/
/* __m128d */

double distance_l2_m128d(__m128d *p1, __m128d *p2, int len)
{
    double ret=0;
    __m128d sum={0,0};

    int i=0;
    for (i=0; i<len/2; i++) {
        __m128d diff;
        diff = _mm_sub_pd(p1[i],p2[i]);
        sum = _mm_add_pd(sum,_mm_mul_pd(diff,diff));
    }

    ret = sum[0] + sum[1];

    for (i*=2; i<len; i++) {
        ret += pow(((double*)p1)[i]-((double*)p2)[i],2);
    }

    return sqrt(ret);
}

double isFartherThan_l2_m128d(__m128d *p1, __m128d *p2, int len, double dist)
{
    double ret=0;
    double dist2=dist*dist;
    __m128d sum={0,0};

    int i=0;
    for (i=0; i<len/2; i++) {
        __m128d diff;
        diff = _mm_sub_pd(p1[i],p2[i]);
        sum = _mm_add_pd(sum,_mm_mul_pd(diff,diff));

        if (i%4==0) {
            if (sum[0]+sum[1] > dist2) {
                return NAN;
            }
        }
    }

    ret = sum[0] + sum[1];

    for (i*=2; i<len; i++) {
        ret += pow(((double*)p1)[i]-((double*)p2)[i],2);
    }

    return sqrt(ret);
}

