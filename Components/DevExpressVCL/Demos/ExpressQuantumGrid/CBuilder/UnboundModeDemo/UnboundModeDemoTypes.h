#include "Classes.hpp"
#include "Forms.hpp"
#include "Windows.hpp"
#include "Graphics.hpp"
#include "SysInit.hpp"
#include "System.hpp"


#ifndef UnboundModeDemoTypesH
#define UnboundModeDemoTypesH

enum TColorScheme { csBlue, csGold, csGreen, csSystem };

typedef DynamicArray<int >  TArrInteger;

typedef DynamicArray<TPoint >  TCells;

typedef TCells *PCells;

typedef DynamicArray<Graphics::TColor >  TColors;

enum TCellState { csClosed, csOpened, csBombMarked, csQuestionMarked };

struct TCellStateRec;
typedef TCellStateRec *PCellStateRec;

struct TCellStateRec
{
  int SurroundNumber;
  TCellState CellState;
} ;

struct TChangedCell;
typedef TChangedCell *PChangedCell;

struct TChangedCell
{
  TPoint Pos;
  TCellStateRec CellState;
};

typedef DynamicArray<TChangedCell>  TChangedCells;

typedef DynamicArray<DynamicArray<TCellStateRec > >  TCellStateRecArrArr;

typedef TCellStateRecArrArr *PCellStateRecArrArr;

typedef DynamicArray<TCellStateRec >  TCellStateRecArr;

typedef TCellStateRecArr *PCellStateRecArr;

enum TGameStatus { gsNew, gsRun, gsLost, gsWon };

typedef TGameStatus *PGameStatus;

struct TGameDifficulty;
typedef TGameDifficulty *PGameDifficulty;

enum TDifficultyType { dtBeginner, dtIntermediate, dtExpert, dtCustom };
struct TGameDifficulty
{
  TDifficultyType DifficultyType;
  int Height;
  int Width;
  int MineCount;
} ;
struct TGameStatusChanged
{
  DynamicArray<TCellStateRec >  CellsToDraw;
  DynamicArray<TPoint >  CellsToDrawPos;
  TGameStatus GameStatus;
  DynamicArray<TPoint >  RedCells;
  TGameDifficulty GameDifficulty;
} ;

typedef TGameStatusChanged *PGameStatusChanged;

enum TMinerFieldActionEventType { meOpenCell, meCloseCell, meBombMarkCell, meQuestionMarkCell, meCheckSurround };

enum TMineCountChangedEventType { mcIncMineCount, mcDecMineCount };

typedef void __fastcall (__closure *TCreateNewGameEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TChangeGameDifficultyEvent)(System::TObject* Sender, const TGameDifficulty &AGameDifficulty);

typedef void __fastcall (__closure *TSrcGameStatusChangedEvent)(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, TChangedCells &AChangedCells, TCells &ARedCells);

typedef void __fastcall (__closure *TIntGameStatusChangedEvent)(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, TCells &AChangedCells, TCells &ARedCells);

typedef void __fastcall (__closure *TFormGameStatusChangedEvent)(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty);

typedef void __fastcall (__closure *TSrcMinerFieldChangedEvent)(System::TObject* Sender, TChangedCells &AChangedCells, TCells &ARedCells);

typedef void __fastcall (__closure *TIntMinerFieldChangedEvent)(System::TObject* Sender, TCells &AChangedCells, TCells &ARedCells);

typedef void __fastcall (__closure *TMinerFieldActionEvent)(System::TObject* Sender, int ACol, int ARow, TMinerFieldActionEventType AMinerFieldEventType);

typedef void __fastcall (__closure *TImageChangedEvent)(System::TObject* Sender, int AImageIndex);

typedef void __fastcall (__closure *TMineCountChangedEvent)(System::TObject* Sender, TMineCountChangedEventType AMineCountChangedEventType);

static const Shortint psBorder = 0x9;
static const Shortint psBoardInnerIndent = 0xf;
static const Shortint psOuterFrameWidth = 0x3;
static const Shortint biNumberHeight = 0x26;
static const Shortint biMineDigitCount = 0x3;
static const Shortint biTimerDigitCount = 0x3;
static const Shortint biButtonWidth = 0x18;
static const Shortint biCountersBorderWidth = 0x1;
static const Shortint biBoardHeight = 0x2d;
static const Shortint imSmile = 0x2;
static const Shortint imAstonisment = 0x3;
static const Shortint imWon = 0x4;
static const Shortint imLost = 0x5;
static const Shortint imBombMark = 0x0;
static const Shortint imQuestionMark = 0x1;
static const Shortint imStruckOutBomb = 0x2;
static const Shortint imRedBomb = 0x3;
static const Shortint imBomb = 0x4;
static const Graphics::TColor clBlueDark = (TColor)0xc56a31;
static const Graphics::TColor clBlueLight = (TColor)0xf7ead9;
static const Graphics::TColor clBlueBright = (TColor)0xff953d;
static const Graphics::TColor clBlueSky = (TColor)0xebc4a4;
static const Graphics::TColor clGold = (TColor)0x47d5fe;
static const Graphics::TColor clGoldDark = (TColor)0x1bdf3;
static const Graphics::TColor clGreyLight = (TColor)0xe2eff1;
static const Graphics::TColor clGreyDark = (TColor)0xb9d9dd;
static const Graphics::TColor clYellowLight = (TColor)0xe1ffff;
static const Graphics::TColor clGreenBright = (TColor)0x82e887;
static const Graphics::TColor clGreenLight = (TColor)0xc9f5cb;
static const Graphics::TColor clGreenObscured = (TColor)0xacf0af;
static const Graphics::TColor clGreenDark = (TColor)0x44dd4b;
static const Graphics::TColor clSilverDark = (TColor)0xa6a6a6;
static const Shortint cliBackground = 0x4;
static const Shortint cliButtonColor = 0x5;
static const Shortint cliFrame3dTopColor = 0x6;
static const Shortint cliFrame3dBottomColor = 0x7;
extern const char* Difficulty;
extern const char* Width;
extern const char* Height;
extern const char* MineCount;
extern const char* Mark;
extern const char* Name1;
extern const char* Name2;
extern const char* Name3;
extern const char* Time1;
extern const char* Time2;
extern const char* Time3;
extern const char* Section;
typedef TColor TSchemeColors[4][9];
extern const TSchemeColors SchemeColors;
void __fastcall MakeArrayFromInt(int AInt, TArrInteger &AArrInt, int MinArrCount = 0x3);
bool __fastcall IsExistsInArray(TCells AArr, int ACol, int ARow);
void __fastcall SetFormPosition(Forms::TForm* AForm, int AXPos, int AYPos);
#endif
