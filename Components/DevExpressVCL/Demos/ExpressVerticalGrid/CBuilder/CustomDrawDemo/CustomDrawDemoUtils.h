#include "Windows.hpp"
#include "cxVGrid.hpp"
#include "Graphics.hpp"

#ifndef CustomDrawDemoUtilsH
#define CustomDrawDemoUtilsH

enum TcxItemCustomDrawType {itNormal, itText, itCell};
enum TCustomDrawingStyle {cdsBkImage, cdsGradient, cdsDefaultDrawing, cdsDependsOnData};
enum TCustomDrawArea {cdaBackground, cdaCategory, cdaCell, cdaHeader};
const int CustomDrawAreaCount = 4;
enum TcxColorScheme {csGrey, csGold, csBlue, csGreen};
const int ColorSchemeCount = 4;
enum  TBkImage {bkiTile, bkiSky, bkiEgypt, bkiMyFace, bkiUserDefined};
const int BkImageCount = 5;

typedef TColor TcxColorSchemeArr[4][3];

const TColor clBlueDark = (TColor)0x00C56A31;
const TColor clBlueLight = (TColor)0x00F7EAD9;
const TColor clBlueBright = (TColor)0x00FF953D;
const TColor clBlueSky = (TColor)0x00EBC4A4;

const TColor clGold = (TColor)0x0047D5FE;
const TColor clGoldDark = (TColor)0x0001BDF3;

const TColor clGreyLight = (TColor)0x00E2EFF1;
const TColor clGreyDark = (TColor)0x00B9D9DD;
const TColor clYellowLight = (TColor)0x00E1FFFF;

const TColor clGreenBright = (TColor)0x0082E887;
const TColor clGreenLight = (TColor)0x00C9F5CB;
const TColor clGreenObscured = (TColor)0x00ACF0AF;
const TColor clGreenDark = (TColor)0x0044DD4B;

const TColor clSilverDark = (TColor)0x00A6A6A6;

//TArrRect = array of TRect;

enum TLineInfo {liTop, liBottom};
typedef Set<CustomDrawDemoUtilsH::TLineInfo, liTop, liBottom> TLineInfos;

extern const TcxColorSchemeArr ColorScheme;
extern const String BkImageResNames[BkImageCount];
extern const String ColorSchemeNames[ColorSchemeCount];
extern const String CustomDrawAreaNames[CustomDrawAreaCount];

void LoadImageFromRes(Graphics::TBitmap* ABitmap, String AResName);

void DrawGradient(TCanvas* ACanvas, TRect ARect, TColor FromColor, TColor ToColor,
  int AStepCount, bool AHorizontal);

void FillRects(TLineInfos ALineInfos, TcxCustomRowHeaderInfo *AHeaderViewInfo,
  TcxCanvas *ACanvas, TColor AColor);

#endif

