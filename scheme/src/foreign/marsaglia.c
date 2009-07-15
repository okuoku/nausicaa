/*
  This  is a  polished version  of George  Marsaglia's test  program for
  pseudo-random number generators.  The code is in the Public Domain.

  To build the program:

    $ gcc -std=c99 -Wall -o marsaglia marsaglia.c

  to run the program:

    $ ./marsaglia

  all the test values should be zero.
*/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>

static void test	(void);
static void test_cong	(void);
static void test_fib	(void);
static void test_kiss	(void);
static void test_lfib4	(void);
static void test_mwc	(void);
static void test_shr3	(void);
static void test_swb	(void);


static const uint32_t t_seed_lfib4[256] = {
  2406566837U,   3945488823U,   3217501373U,   1294085848U,
  1685356791U,   1879998683U,    354155033U,   1162810413U,
  2655137063U,    226356688U,   3472744942U,    543082922U,
  3762916352U,   1247145318U,   1960484516U,   2834937135U,
  3556413474U,   4015588494U,   2433866612U,   1542681415U,
   837890377U,   2156031093U,    641728713U,   2071218145U,
  3128597539U,   2092573851U,   1548155322U,   3718870319U,
  4167822943U,   2912646175U,   2911643863U,   3362985712U,
  1886528418U,    592629127U,   1204737837U,   1592327664U,
  3126732962U,      1554786U,   1906632076U,   2492476552U,
   111002753U,   4108454543U,   3793230690U,   4226138624U,
   422644314U,   2797766512U,    847647967U,   1289123979U,
  1969535464U,   1902370438U,    974706952U,   4222545502U,
  1130793588U,   3094143516U,     86711091U,   1749721364U,
   278301801U,   2154688974U,   2866252662U,   1487174760U,
   119646572U,   3549236133U,    302009453U,    162239274U,
  1776703449U,   1318650385U,   3594113676U,   4020467310U,
   941062378U,   1823539007U,   3848290724U,   1275281653U,
  4013456607U,   2787872658U,    770103194U,   2457950900U,
  3336084503U,    657199110U,   1723679164U,    798896722U,
  1705823163U,   1383115390U,   2041707688U,   3804770597U,
  1136092689U,   2827291170U,   1227628224U,    221271644U,
  1977255824U,   1852155101U,   2301247803U,   3595646145U,
  3709922751U,   2732837192U,    529978376U,   1840394256U,
   850198089U,   2335427956U,   3449558694U,   3023230479U,
   868614440U,   1099611446U,   3334890752U,   4190589169U,
  3993307947U,   3680421264U,   3143497825U,    930612474U,
  3805940850U,   2814955303U,   2609714583U,   3412337804U,
  1947870458U,   1173915461U,   1573012014U,    927925469U,
  1957387227U,   2823082061U,   3013874876U,   1144690729U,
   321153706U,   3505341913U,   2118050097U,   1431882057U,
  3398744687U,   2232621844U,   3712718609U,   2146053627U,
   159722353U,   3930544785U,   1289237494U,   3622337738U,
   132620258U,   3395020004U,   3971404583U,   2672497632U,
   418556265U,    375722238U,   4083798982U,   3832998408U,
  4013463868U,   1778758890U,   3900234414U,   3865137622U,
     7986325U,   3745270548U,   1264572959U,   1918227621U,
  2293215037U,   4013539679U,   1728047317U,   1329424085U,
  3875557031U,   3025492221U,    424037964U,   3308379748U,
   463014440U,   2869036490U,   3540996349U,    378750900U,
  1776914604U,   2246544134U,   1500424097U,   1031318071U,
   653625805U,   3003062951U,    950363772U,   1878098560U,
  1705309242U,   3066444961U,   2718370657U,   2326258461U,
  3630708903U,    987445312U,    187170730U,    958728662U,
  4026066647U,   4126946950U,   1809706436U,   2404651296U,
  2208247683U,   3582265286U,   2071240021U,   2116044308U,
  2716786358U,   2335209520U,   2975196056U,    245850675U,
  2274606232U,    801725457U,   4131433647U,    291882028U,
   173368002U,    211292240U,   1606934101U,     86222388U,
  2744096089U,   2594669165U,   2583936776U,    267153027U,
  1270120814U,   3848285866U,   3106929809U,   3457673136U,
  4059379692U,   2694564428U,   3420145872U,   4000069744U,
   186853964U,   1294043757U,    445382178U,   1508441456U,
   975087012U,   1239667053U,    827019081U,   2586195652U,
  2163669318U,   1252742899U,    221573984U,      9342999U,
  3375791150U,    813492546U,    881353819U,   1507495170U,
  4208776213U,   3297654913U,   3373356384U,   2245647143U,
  3413689312U,   2468442767U,   3233866900U,   2119481006U,
  3323181420U,    275426611U,    267195229U,   3023950404U,
  2970782157U,   3409642083U,   1313209854U,   3473951044U,
  3633718954U,   3189450566U,   1981092696U,    708163582U,
   488758822U,   2882757362U,   3437433893U,   1453523484U,
  3414277213U,   1339141620U,   3946001167U,    103943150U,
  1689000206U,   2735316516U,    916302334U,    823403292U
};


static const uint32_t t_seed_swb[256] = {
  3444715520U,    589627959U,   1852145839U,   1474265681U,
  1993127274U,   2857940909U,   2250181688U,   1758786885U,
  2648200516U,   3869276561U,   2131109435U,   3448185628U,
  1031138409U,   1251313198U,   3212840128U,   2372025478U,
  3036010335U,    804934505U,   3911956942U,   2392823437U,
  1006134078U,   1365723494U,   1825619600U,   2362330954U,
  3662377205U,     47556407U,    927442372U,   1809363575U,
    50559844U,    535744360U,   2869379169U,   3507056475U,
  4028552258U,   4084264384U,   3545253694U,    727759159U,
  2654534117U,   3533850408U,    304476320U,   1764902585U,
  3679535483U,   1403824263U,    834926528U,   3713675177U,
  2170354971U,   4114394043U,    130927612U,    282952029U,
  2968809523U,   3602074780U,   3693234238U,   3935779525U,
  4252866265U,   1395277872U,    667163033U,   1147112852U,
  1587818444U,    792412337U,    204831206U,   1551942735U,
  2359110429U,   4196550158U,   2674773592U,   4293179457U,
  1064612766U,   1513436951U,   3188416084U,   3881802103U,
  1948692362U,   3490069274U,   1862523903U,   1095295803U,
   351710150U,   2463825887U,   3707440418U,   1512411940U,
   319358280U,   1920961757U,   2779542203U,   1485468529U,
  3495985109U,   1286668255U,   1351892342U,   1520856886U,
  3299209310U,   3214982838U,   2677774711U,   1526350562U,
  1760324146U,   2081481100U,   3799603074U,    149889485U,
  3076011032U,   2859215584U,   3154421752U,   1815946240U,
   347370820U,    737664179U,   3237266725U,   2407857342U,
  3579310210U,   2547778315U,   2049832610U,   2137532637U,
  3529594535U,   3780058177U,   3589887133U,   1372294963U,
  2064299265U,   4074784224U,   1313827789U,   2964267889U,
  4273698787U,   3462965074U,   2664897851U,   4253389708U,
  1039607481U,   3278618941U,   3090152390U,   3824433171U,
  3271035935U,   2539015026U,   2046789971U,    709477519U,
   433921941U,    750683020U,   3531235443U,   3308066230U,
   612637020U,   1706458371U,   3446844314U,   3726803551U,
   553043350U,   1482744084U,   1347662155U,   1305578732U,
  3594677322U,   1489645083U,   1959332212U,    437229145U,
   187782346U,   1781868579U,     27681368U,   1400042057U,
  1118846718U,   3345622882U,   2272713889U,   2978416100U,
   615822425U,   4067047814U,    826286420U,   2239310652U,
  3681231216U,   3105564630U,    214685676U,   4045994565U,
   308613449U,   1398994572U,   2030420609U,   3333349787U,
  1617601581U,   3302743289U,   4041150887U,   2007052644U,
  1265002717U,   1859030124U,   1386709160U,   3190496627U,
  3676023138U,   3142527399U,   4136630278U,   1553025939U,
    79421320U,   1550860049U,   3293858257U,   1864730241U,
   742026642U,    301833874U,   3979643423U,   3265481090U,
  2096184672U,   2309979351U,   3241215696U,   1154856658U,
   819869785U,   1858497240U,    923479652U,   2598431294U,
  2065267425U,   2909206677U,   1606335486U,   3334236076U,
  3934221624U,   3419762829U,   3875133691U,    382578678U,
  3192117824U,    505678928U,   3898267352U,     95968481U,
  3699565520U,    882202361U,   4293193763U,   3918914979U,
  3313751816U,   3709105074U,   2794105100U,   3241694746U,
   942070142U,   2832737836U,    614516447U,    353963846U,
  1479714362U,   2758563050U,   3289904971U,   1039663995U,
   370394114U,    176879025U,   3590204376U,    160913434U,
  2350559687U,    873237449U,   1477310632U,   3727399863U,
  1801381754U,   2548083017U,   2668966345U,   1372528365U,
  3691739586U,   1279645001U,   3754587333U,   2278192398U,
  1433520522U,   4115832233U,   3056969095U,   2295443016U,
  1657648077U,    843630300U,   2473935165U,   4018708818U,
  2158081974U,    470260008U,   3001227663U,    412458272U,
  1722527672U,   1861980050U,   1730243661U,   1161583444U,
   776245256U,   2000435315U,   2675310267U,    133667017U,
   762714965U,    993364084U,   2316992894U,   1904476135U
};


/* Remember that "^" is the XOR operator. */

#define znew	(z = 36969 * (z & 65535) + (z >> 16))

#define wnew	(w = 18000 * (w & 65535) + (w >> 16))

#define MWC	((znew << 16) + wnew)

#define SHR3	(jsr ^= (jsr << 17),	\
		 jsr ^= (jsr >> 13),	\
		 jsr ^= (jsr << 5))

#define CONG	(jcong = 69069 * jcong + 1234567)

		/* Here there is overflow for 32 bits! */
#define FIB	((b = a + b),		\
		 (a = b - a))

#define KISS	((MWC ^ CONG) + SHR3)

#define LFIB4	(c++,			\
		 t[c] = t[c]		\
		      + t[UC(c +  58)]	\
		      + t[UC(c + 119)]	\
		      + t[UC(c + 178)])

#define SWB	(c++,				\
		 bro  = (x < y),		\
		 t[c] = (x = t[UC(c + 34)])	\
		      - (y = t[UC(c + 19)]	\
			   + bro))

#define UNI	(KISS * 2.328306e-10)

#define VNI	(((long) KISS) * 4.656613e-10)

		/* A cast operation to make  the index "c" modulo 256 in
		   the buffer "t[256]" of LFIB4 and SWB. */
#define UC	(uint8_t)


int
main (void)
{
  test();
  test_cong();
  test_fib();
  test_kiss();
  test_lfib4();
  test_mwc();
  test_shr3();
  test_swb();
  exit(EXIT_SUCCESS);
}

static void
test (void)
/* This is a test program.  It should compile and print 7 zeros. */
{
  uint32_t k;

  /* seed values */
  uint32_t z     = 12345;
  uint32_t w     = 65435;
  uint32_t jsr   = 34221;
  uint32_t jcong = 12345;
  uint32_t a     = 9983651;
  uint32_t b     = 95746118;
  uint32_t t[256];

  /* other state values */
  uint8_t  c     = 0;
  uint32_t x     = 0;
  uint32_t y     = 0;
  uint32_t bro   = 0;


  printf("*** Tests for George Marsaglia's PRNGs.\n");

  for(int i=0; i<256; ++i)
    t[i] = KISS;

  printf("\n** Seed values\n");
  printf("a     = %u\n", a);
  printf("b     = %u\n", b);
  printf("jsr   = %u\n", jsr);
  printf("jcong = %u\n", jcong);
  printf("w     = %u\n", w);
  printf("z     = %u\n", z);

#if 0
  printf("t_lfib4\n");
  for (int i=0; i<64; ++i) {
    for (int j=0; j<4; ++j) {
      printf("%12u, ", t[i*4+j]);
    }
    printf("\n");
  }
#endif

  printf("\n*** Starting tests\n");

  /* ========== WARNING ==========

     DO NOT CHANGE THE ORDER OF THE TESTS BELOW
     MANY TESTS USE VALUES PRODUCES BY PREVIOUS TESTS
  */

  printf("\n** testing LFIB4\n");
  printf("c   = %u\n", c);
  for(int i=1; i<1000001; i++)
    k = LFIB4;
  printf("\tlast %u\n\ttest %u\n", k, k-1064612766U);

  printf("\n** testing SWB\n");
  printf("c   = %u\n", c);
  printf("x   = %u\n", x);
  printf("y   = %u\n", y);
  printf("bro = %u\n", bro);
#if 0
  printf("\nt_SWB\n");
  for (int i=0; i<64; ++i) {
    for (j=0; j<4; ++j) {
      printf("%12u, ", t[i*4+j]);
    }
    printf("\n");
  }
#endif
  for(int i=1; i<1000001; i++)
    k = SWB;
  printf("\tlast %u\n\ttest %u\n", k, k-627749721U);

  printf("\n** testing KISS\n");
  printf("jcong = %u\n", jcong);
  printf("jsr   = %u\n", jsr);
  printf("w     = %u\n", w);
  printf("z     = %u\n", z);
  for(int i=1; i<1000001; i++)
    k = KISS;
  printf("\tlast %u\n\ttest %u\n", k, k-1372460312U);

  printf("\n** testing CONG\n");
  printf("jcong = %u\n", jcong);
  for(int i=1; i<1000001; i++)
    k = CONG;
  printf("\tlast %u\n\ttest %u\n", k, k-1529210297U);

  printf("\n** testing SHR3\n");
  printf("jsr   = %u\n", jsr);
  for(int i=1; i<1000001; i++)
    k = SHR3;
  printf("\tlast %u\n\ttest %u\n", k, k-2642725982U);

  printf("\n** testing MWC\n");
  printf("w     = %u\n", w);
  printf("z     = %u\n", z);
  for(int i=1; i<1000001; i++)
    k = MWC;
  printf("\tlast %u\n\ttest %u\n", k, k-904977562U);

  printf("\n** testing FIB\n");
  printf("a     = %u\n", a);
  printf("b     = %u\n", b);
  for(int i=1; i<1000001; i++)
    k = FIB;
  printf("\tlast %u\n\ttest %u\n", k, k-3519793928U);
}

static void
test_cong (void)
{
  uint32_t	jcong_seed = 2524969849U;
  uint32_t	jcong = jcong_seed;
  uint32_t	k;

  for(int i=1; i<1000001; i++)
    k = CONG;
  printf("\nResult of CONG: last %u, test %u\n", k, k-1529210297U);
  printf("\tjcong = %u\n", jcong_seed);
}

static void
test_fib (void)
{
  uint32_t	a_seed = 9983651, b_seed = 95746118;
  uint32_t	a = a_seed, b = b_seed;
  uint32_t	k;

  for(int i=1; i<1000001; i++)
    k = FIB;
  printf("\nResult of FIB: last %u, test %u\n", k, k-3519793928U);
  printf("\ta = %u\n", a_seed);
  printf("\tb = %u\n", b_seed);
}

static void
test_kiss (void)
{
  uint32_t
    jcong_seed = 1017008441U,
    jsr_seed   = 3259917390U,
    w_seed     = 99545079U,
    z_seed     = 2247183469U;
  uint32_t
    jcong = jcong_seed,
    jsr   = jsr_seed,
    w     = w_seed,
    z     = z_seed,
    k;

  for(int i=1; i<1000001; i++)
    k = KISS;
  printf("\nResult of KISS: last %u, test %u\n", k, k-1372460312U);
  printf("\tjcong = %u\n", jcong_seed);
  printf("\tjsr   = %u\n", jsr_seed);
  printf("\tw     = %u\n", w_seed);
  printf("\tz     = %u\n", z_seed);
}

static void
test_lfib4 (void)
{
  uint32_t	t[256];
  uint8_t	c = 0;
  uint32_t	k;

  for (int i=0; i<256; ++i)
    t[i] = t_seed_lfib4[i];

  for(int i=1; i<1000001; i++)
    k = LFIB4;
  printf("\nResult of LFIB4: last %u, test %u\n", k, k-1064612766U);
}

static void
test_mwc (void)
{
  uint32_t	w_seed = 1046675282, z_seed = 2374144069;
  uint32_t	w = w_seed, z = z_seed;
  uint32_t	k;

  for(int i=1; i<1000001; i++)
    k = MWC;
  printf("\nResult of MWC: last %u, test %u\n", k, k-904977562U);
  printf("\tw = %u\n", w_seed);
  printf("\tz = %u\n", z_seed);
}

static void
test_shr3 (void)
{
  uint32_t	jsr_seed = 4176875757U;
  uint32_t	jsr = jsr_seed;
  uint32_t	k;

  for(int i=1; i<1000001; i++)
    k = SHR3;
  printf("\nResult of SHR3: last %u, test %u\n", k, k-2642725982U);
  printf("\tjsr = %u\n", jsr_seed);
}

static void
test_swb (void)
{
  uint32_t	t[256];
  uint8_t	c, c_seed = 64;
  uint32_t	x   = 0;
  uint32_t	y   = 0;
  uint32_t	bro = 0;
  uint32_t	k;

  c = c_seed;
  for (int i=0; i<256; ++i)
    t[i] = t_seed_swb[i];

  for(int i=1; i<1000001; i++)
    k = SWB;
  printf("\nResult of SWB: last %u, test %u\n", k, k-627749721U);
  printf("\tc = %u\n", c_seed);
}

/* end of file */
