unit UnboundModeDemoTypes;

interface

uses Graphics, Windows, Forms, Classes;

const
  psBorder = 9;
  psBoardInnerIndent = 15;
  psOuterFrameWidth = 3;
  biNumberHeight = 38;
  biMineDigitCount = 3;
  biTimerDigitCount = 3;
  biButtonWidth = 24;
  biCountersBorderWidth = 1;
  biBoardHeight = 45;

  imSmile = 2;
  imAstonisment = 3;
  imWon = 4;
  imLost = 5;

  imBombMark = 0;
  imQuestionMark = 1;
  imStruckOutBomb = 2;
  imRedBomb = 3;
  imBomb = 4;

  clBlueDark = TColor($00C56A31);
  clBlueLight = TColor($00F7EAD9);
  clBlueBright = TColor($00FF953D);
  clBlueSky = TColor($00EBC4A4);

  clGold = TColor($0047D5FE);
  clGoldDark = TColor($0001BDF3);

  clGreyLight = TColor($00E2EFF1);
  clGreyDark = TColor($00B9D9DD);
  clYellowLight = TColor($00E1FFFF);

  clGreenBright = TColor($0082E887);
  clGreenLight = TColor($00C9F5CB);
  clGreenObscured = TColor($00ACF0AF);
  clGreenDark = TColor($0044DD4B);

  clSilverDark = TColor($00A6A6A6);

  cliBackground = 4;
  cliButtonColor = 5;
  cliFrame3dTopColor = 6;
  cliFrame3dBottomColor = 7;

  Difficulty = 'Difficulty';
  Width = 'Width';
  Height = 'Height';
  MineCount = 'MineCount';
  Mark = 'Mark';
  Name1 = 'Name1';
  Name2 = 'Name2';
  Name3 = 'Name3';
  Time1 = 'Time1';
  Time2 = 'Time2';
  Time3 = 'Time3';
  Section: String = 'Software\Developer Express\UnboundModeDemo';

type
  TColorScheme = (csBlue, csGold, csGreen, csSystem);
  TSchemeColors = array [0..3, 0..8] of TColor;

const
  SchemeColors: TSchemeColors = ((clBlueBright, clBlueLight, clWhite, clBlueDark, clBlueSky, clBlueSky,   clBlueDark, clWhite, clBlueLight),
                                 (clGold,       clGreyLight, clWhite, clGoldDark, clGreyDark, clGreyDark, clGoldDark, clWhite, clGreyLight),
                                 (clGreenBright, clGreenLight, clWhite, clGreenDark, clGreenObscured, clGreenLight, clGreenDark, clWhite, clGreenLight),
                                 (clSilverDark, clSilver, clWhite, clGray, clSilver, clSilver, clGray, clWhite, clSilver));

type
  TArrInteger = array of Integer;
  PCells = ^TCells;
  TCells = array of TPoint;

  TColors = array of TColor;

  TCellState = (csClosed, csOpened, csBombMarked, csQuestionMarked);

  PCellStateRec =^ TCellStateRec;

  TCellStateRec = record
    SurroundNumber: Integer;
    CellState: TCellState;
  end;

  PChangedCell = ^TChangedCell;
  TChangedCell = record
    Pos: TPoint;
    CellState: TCellStateRec;
  end;

  TChangedCells = array of TChangedCell;


  PCellStateRecArrArr = ^TCellStateRecArrArr;
  TCellStateRecArrArr = array of array of TCellStateRec;

  PCellStateRecArr = ^TCellStateRecArr;
  TCellStateRecArr = array of TCellStateRec;

  PGameStatus = ^TGameStatus;
  TGameStatus = (gsNew, gsRun, gsLost, gsWon);

  PGameDifficulty = ^TGameDifficulty;
  TDifficultyType = (dtBeginner, dtIntermediate, dtExpert, dtCustom);

  TGameDifficulty = record
    DifficultyType: TDifficultyType;
    Height: Integer;
    Width: Integer;
    MineCount: Integer;
  end;

  PGameStatusChanged = ^TGameStatusChanged;

  TGameStatusChanged = record
    CellsToDraw: TCellStateRecArr;
    CellsToDrawPos: TCells;
    GameStatus: TGameStatus;
    RedCells: TCells;
    GameDifficulty: TGameDifficulty;
  end;

type TMinerFieldActionEventType = (meOpenCell, meCloseCell, meBombMarkCell, meQuestionMarkCell, meCheckSurround);
type TMineCountChangedEventType = (mcIncMineCount, mcDecMineCount);

procedure SetFormPosition(AForm: TForm; AXPos, AYPos: Integer);
function IsExistsInArray(AArr: TCells; ACol, ARow: Integer): Boolean;

procedure MakeArrayFromInt(AInt: Integer; var AArrInt: TArrInteger; MinArrCount: Integer = 3);

type TCreateNewGameEvent = TNotifyEvent;
type TChangeGameDifficultyEvent = procedure (Sender: TObject; const AGameDifficulty: TGameDifficulty) of Object;

type TSrcGameStatusChangedEvent = procedure (Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty; var AChangedCells: TChangedCells; var ARedCells: TCells) of Object;
type TIntGameStatusChangedEvent = procedure (Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty; var AChangedCells: TCells; var ARedCells: TCells) of Object;
type TFormGameStatusChangedEvent = procedure (Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty) of Object;

type TSrcMinerFieldChangedEvent = procedure (Sender: TObject; var AChangedCells: TChangedCells; var ARedCells: TCells) of Object;
type TIntMinerFieldChangedEvent = procedure (Sender: TObject; var AChangedCells: TCells; var ARedCells: TCells) of Object;

type TMinerFieldActionEvent = procedure (Sender: TObject; ACol, ARow: Integer; AMinerFieldEventType: TMinerFieldActionEventType) of Object;
type TImageChangedEvent = procedure (Sender: TObject; AImageIndex: Integer) of Object;
type TMineCountChangedEvent = procedure (Sender: TObject; AMineCountChangedEventType: TMineCountChangedEventType) of Object;

implementation

uses Sysutils;

procedure MakeArrayFromInt(AInt: Integer; var AArrInt: TArrInteger; MinArrCount: Integer = 3);
var
  ind: Integer;
begin
  SetLength(AArrInt, MinArrCount);
  Ind := 0;
  while AInt <> 0 do
  begin
    if Ind >= MinArrCount then SetLength(AArrInt, Length(AArrInt)+1);
    AArrInt[Ind] := AInt mod 10;
    AInt := AInt div 10;
    Inc(Ind);
  end;
end;

function IsExistsInArray(AArr: TCells; ACol, ARow: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to High(AArr) do
    if (AArr[i].x = ACol) and (AArr[i].y = ARow) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure SetFormPosition(AForm: TForm; AXPos, AYPos: Integer);
var
{$IFNDEF CLR}
  pWorkArea: PRect;
{$ENDIF}
  WorkArea: TRect;
begin
{$IFDEF CLR}
  SystemParametersInfo(SPI_GETWORKAREA, 0,  WorkArea, 0);
{$ELSE}
  New(pWorkArea);
  SystemParametersInfo(SPI_GETWORKAREA, 0,  pWorkArea, 0);
  WorkArea := pWorkArea^;
  Dispose(pWorkArea);
{$ENDIF}

  with AForm do
  begin
    if (Owner as TForm).Left + Width + AXPos > WorkArea.Right then
      Left := WorkArea.Right - Width else
    if (Owner as TForm).Left + AXPos < WorkArea.Left then
      Left := WorkArea.Left else
    Left := (Owner as TForm).Left + AXPos;

    if (Owner as TForm).Top + Height + AYPos > WorkArea.Bottom then
      Top := WorkArea.Bottom - Height - 4 else
    Top := (Owner as TForm).Top + AYPos;
  end;
end;

end.
