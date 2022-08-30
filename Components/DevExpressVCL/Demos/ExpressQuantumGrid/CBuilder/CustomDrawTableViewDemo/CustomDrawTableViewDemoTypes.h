#include "Windows.hpp"
#include "Graphics.hpp"

#ifndef CustomDrawTableViewDemoTypesH
#define CustomDrawTableViewDemoTypesH

enum TCustomDrawingStyle {cdsBkImage, cdsGradient, cdsDependsOnData, cdsDefaultDrawing};
typedef Set<TCustomDrawingStyle, cdsBkImage, cdsDefaultDrawing> TCustomDrawingStyles;
enum TCustomDrawArea {cdaCell, cdaColumnHeader, cdaFooterCell, cdaGroupCell, cdaIndicatorCell, cdaPartBackGround};
enum TViewType {vtMaster, vtDetail};
enum TColorScheme {csGrey, csGold, csBlue, csGreen};

enum  TBkImage {bkiTile, bkiSky, bkiEgypt, bkiMyFace, bkiUserDefined};
typedef TBkImage TBkImages[2][6];

typedef CustomDrawTableViewDemoTypesH::TColorScheme TColorSchemes[2][6];
typedef TColor SchemeArr[4][3];
typedef TCustomDrawingStyle TCustomDrawingStyleArr[2][6];
typedef Graphics::TBitmap TUserDefinedBitMaps[2][6];
typedef TFont* TFonts[2][6];

struct TCustomDrawItem;
typedef TCustomDrawItem *PCustomDrawItem;

struct TCustomDrawItem {
   TViewType ViewType;
   TCustomDrawArea CustomDrawArea;
};

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

extern const SchemeArr ColorScheme;

void DrawGradient(TCanvas* ACanvas, TRect ARect, TColor FromColor, TColor ToColor,
  int AStepCount, bool IsVertical);

void LoadImageFromRes(Graphics::TBitmap* ABitmap, String AResName);

#endif
