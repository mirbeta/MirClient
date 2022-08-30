unit UnboundModeDemoMinerCore;

interface

uses
{$IFDEF CLR}
  Types,
{$ENDIF}
  UnboundModeDemoTypes;

type
  TMinerField = class(TObject)
  private
    FHeight: Integer;
    FWidth: Integer;
    FMineCount: Integer;
    FCellState: TCellStateRecArrArr;
    FGameDifficulty: TGameDifficulty;
    FGameStatus: TGameStatus;
    FCellsBombMarkedCount: Integer;
    FRedCells: TCells;
    FMinerFieldChanged: TSrcMinerFieldChangedEvent;
    FGameStatusChanged: TSrcGameStatusChangedEvent;
    function CheckFieldBounds(AXPos, AYPos: Integer): Boolean;

    function Get_Height: Integer;
    procedure Set_Height(const Value: Integer);

    function Get_Widht: Integer;
    procedure Set_Width(const Value: Integer);

    function Get_MineCount: Integer;
    procedure Set_MineCount(const Value: Integer);

    function Get_CellState(XIndex, YIndex: Integer): TCellStateRec;
    procedure Set_CellState(XIndex, YIndex: Integer; const Value: TCellStateRec);

    function Get_GameDifficulty: TGameDifficulty;
    procedure Set_GameDifficulty(const Value: TGameDifficulty);

    procedure OpenSurround(AXPos, AYPos: Integer; var AChangedCells: TCells);
    function Get_GameStatus: TGameStatus;
    procedure Set_GameStatus(const Value: TGameStatus);

    procedure CreateNewGame;
    function IsGameFinished: Boolean;


    procedure FireEvGameStatusChanged(AGameStatus: TGameStatus; var AChangedCells: TCells);
    procedure FireEvMinerFieldChanged(var AChangedCells: TCells);

    property GameDifficulty: TGameDifficulty read Get_GameDifficulty write Set_GameDifficulty;
    property CellState[XIndex, YIndex: Integer]: TCellStateRec read Get_CellState write Set_CellState;
    property Height: Integer read Get_Height write Set_Height;
    property Width: Integer read Get_Widht write Set_Width;
    property MineCount: Integer read Get_MineCount write Set_MineCount;
    property GameStatus: TGameStatus read Get_GameStatus write Set_GameStatus;
    procedure FillBombCells(var AChangedCells: TCells);
  protected
    procedure DropMines(AFirstX, AFirstY: Integer); virtual;
    procedure OpenCell(AXPos, AYPos: Integer); virtual;
    procedure BombMarkCell(AXPos, AYPos: Integer); virtual;
    procedure QuestionMarkCell(AXPos, AYPos: Integer); virtual;
    procedure CloseCell(AXPos, AYPos: Integer); virtual;
    procedure CheckSurround(AXPos, AYPos: Integer); virtual;
  public
    property OnMinerFieldChanged: TSrcMinerFieldChangedEvent read FMinerFieldChanged write FMinerFieldChanged;
    property OnGameStatusChanged: TSrcGameStatusChangedEvent read FGameStatusChanged write FGameStatusChanged;

    constructor Create; overload;
    destructor Destroy; override;
    { Event handlers }
    procedure HandleEvCreateNewGame(Sender: TObject);
    procedure HandleEvChangeGameDifficulty(Sender: TObject; const AGameDifficulty: TGameDifficulty);
    procedure HandleMinerFieldActionEvent(Sender: TObject; ACol, ARow: Integer; AMinerFieldEventType: TMinerFieldActionEventType);
  end;

var
  MinerField: TMinerField;

implementation

uses Dialogs, SysUtils, Windows, Classes;

{ TMinerField }

constructor TMinerField.Create;
begin
  inherited Create;
  FGameStatus := gsNew;
end;

destructor TMinerField.Destroy;
begin
  FRedCells := nil;
  FCellState := nil;
  inherited Destroy;
end;

function TMinerField.Get_Height: Integer;
begin
  Result := FHeight;
end;

procedure TMinerField.Set_Height(const Value: Integer);
begin
  if (9 <= Value) and (Value <= 24) then
    FHeight := Value else
  if Value < 9 then
    FHeight := 9 else
  if 24 < Value then
    FHeight := 24;
end;

function TMinerField.Get_Widht: Integer;
begin
  Result := FWidth;
end;

procedure TMinerField.Set_Width(const Value: Integer);
begin
  if (9 <= Value) and (Value <= 30) then
    FWidth := Value else
  if (Value < 9) then
    FWidth := 9 else
  if 30 < Value then
    FWidth := 30;
end;

function TMinerField.Get_MineCount: Integer;
begin
  Result := FMineCount;
end;

procedure TMinerField.Set_MineCount(const Value: Integer);
begin
  if (10 <= Value) and (Value <= (FHeight - 1)*(FWidth - 1)) then
    FMineCount := Value else
  if Value < 10 then
    FMineCount := 10 else
  if (FHeight - 1)*(FWidth - 1) < Value then
    FMineCount := (FHeight - 1)*(FWidth - 1);
end;

function TMinerField.Get_CellState(XIndex, YIndex: Integer): TCellStateRec;
begin
  Result := FCellState[XIndex, YIndex];
end;

procedure TMinerField.Set_CellState(XIndex, YIndex: Integer;
  const Value: TCellStateRec);
begin
  if (FCellState[XIndex, YIndex].SurroundNumber <> Value.SurroundNumber) or
    (FCellState[XIndex, YIndex].CellState <> Value.CellState) then
    begin
      if FCellState[XIndex, YIndex].CellState = csBombMarked then
        Dec(FCellsBombMarkedCount);

      FCellState[XIndex, YIndex] := Value;
      if Value.CellState = csBombMarked then
        Inc(FCellsBombMarkedCount);
    end;
end;

procedure TMinerField.CheckSurround(AXPos, AYPos: Integer);
var
  WrongBombMark: Boolean;
  ChangedCells: TCells;
  function IsCountBombMarkedCorrect(AXPos, AYPos: Integer): Boolean;
  var
    RealBombCount: Integer;
    BombMarked: Integer;
    i, j: Integer;
  begin
    BombMarked := 0;
    RealBombCount := 0;
    for i:=-1 to 1 do
      for j:=-1 to 1 do
        if CheckFieldBounds(AXPos+i, AYPos+j) then
        begin
          if FCellState[AXPos+i, AYPos+j].CellState = csBombMarked then
          begin
            Inc(BombMarked);
            if FCellState[AXPos+i, AYPos+j].SurroundNumber <> -1 then
              WrongBombMark := True
          end;
          if FCellState[AXPos+i, AYPos+j].SurroundNumber = -1 then
          begin
            SetLength(FRedCells, Length(FRedCells)+1);
            FRedCells[High(FRedCells)].x := AXPos+i;
            FRedCells[High(FRedCells)].y := AYPos+j;
            Inc(RealBombCount);
          end;
        end;
    Result := RealBombCount = BombMarked;
  end;
begin
  if (FCellState[AXPos, AYPos].CellState = csBombMarked) or
    (FCellState[AXPos, AYPos].CellState = csClosed) or
    (FCellState[AXPos, AYPos].CellState = csQuestionMarked) then
    Exit;

  // check whether csBombMarked Cells set well
  WrongBombMark := False;
  if not IsCountBombMarkedCorrect(AXPos, AYPos) then
  begin
    FRedCells := nil;
    Exit;
  end;

  // open surrounding csClosed Cells
  OpenSurround(AXPos, AYPos, ChangedCells);

  if WrongBombMark then
  begin
    FGameStatus := gsLost;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsLost, ChangedCells);
    Exit;
  end;
  FRedCells := nil;

  FireEvMinerFieldChanged(ChangedCells);
  if IsGameFinished then
  begin
    FGameStatus := gsWon;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsWon, ChangedCells);
  end;
end;

procedure TMinerField.CreateNewGame;
var
  i, j: Integer;
begin
  FCellsBombMarkedCount := 0;
  FRedCells := nil;
  FCellState := nil;
  SetLength(FCellState, FWidth);
  for i:=0 to FWidth - 1 do
  begin
    SetLength(FCellState[i], FHeight);
    for j:=0 to FHeight - 1 do
      FCellState[i, j].CellState := csClosed;
  end;
end;

procedure TMinerField.FillBombCells(var AChangedCells: TCells);
var
  i, j: Integer;
begin
  for i:=0 to High(FCellState) do
    for j:=0 to High(FCellState[i]) do
      if FCellState[i,j].SurroundNumber = -1 then
      begin
        SetLength(AChangedCells, Length(AChangedCells) + 1);
        AChangedCells[High(AChangedCells)] := Point(i, j);
      end;
end;

procedure TMinerField.OpenCell(AXPos, AYPos: Integer);
var
  ChangedCells: TCells;
begin
  if GameStatus = gsNew then
  begin
    DropMines(AXPos, AYPos);
    FGameStatus := gsRun;
    FireEvGameStatusChanged(gsRun, ChangedCells);
  end;

  if FCellState[AXPos, AYPos].CellState = csBombMarked then
    Exit;
  if FCellState[AXPos, AYPos].SurroundNumber = -1 then
  begin
    // Stop current game
    FGameStatus := gsLost;
    SetLength(FRedCells, Length(FRedCells)+1);
    FRedCells[0].x := AXPos; FRedCells[0].y := AYPos;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsLost, ChangedCells);
    Exit;
  end;

  if FCellState[AXPos, AYPos].SurroundNumber <> 0 then
  begin
    FCellState[AXPos, AYPos].CellState := csOpened;
    SetLength(ChangedCells, 1);
    ChangedCells[0].x := AXPos; ChangedCells[0].y := AYPos;
  end
  else
    OpenSurround(AXPos, AYPos, ChangedCells);
  FireEvMinerFieldChanged(ChangedCells);

  if IsGameFinished then
  begin
    FGameStatus := gsWon;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsWon, ChangedCells);
  end;
end;

function TMinerField.Get_GameDifficulty: TGameDifficulty;
begin
  Result := FGameDifficulty;
end;

procedure TMinerField.Set_GameDifficulty(const Value: TGameDifficulty);
var
  ACells: TCells;
  procedure SetThisDifficulty;
  begin
    FGameDifficulty.Height := Height;
    FGameDifficulty.Width := Width;
    FGameDifficulty.MineCount := MineCount;
  end;
begin
  FGameDifficulty.DifficultyType := Value.DifficultyType;
  case Value.DifficultyType of
    dtBeginner:
    begin
      Height := 9;
      Width := 9;
      MineCount := 10;
    end;
    dtIntermediate:
    begin
      Height := 16;
      Width := 16;
      MineCount := 40;
    end;
    dtExpert:
    begin
      Height := 16;
      Width := 30;
      MineCount := 99;
    end;
    dtCustom:
    begin
      Height := Value.Height;
      Width := Value.Width;
      MineCount := Value.MineCount;
    end;
  end;
  SetThisDifficulty;
  GameStatus := gsNew;
  FireEvGameStatusChanged(gsNew, ACells);
end;

procedure TMinerField.DropMines(AFirstX, AFirstY: Integer);
var
  DeadNumber: Integer;
  DroppedMines: Integer;
  RandomBase: Integer;
  DroppedNumber: Integer;
  XPos, YPos: Integer;
  procedure SetSurround(AXPos, AYPos: Integer);
  var
    i, j: Integer;
  begin
    for i:=-1 to 1 do
      for j:=-1 to 1 do
        if CheckFieldBounds(AXPos+i, AYPos+j) and (FCellState[AXPos+i, AYPos+j].SurroundNumber <> -1) then
          Inc(FCellState[AXPos+i, AYPos+j].SurroundNumber);
  end;
begin
  DeadNumber := FWidth * AFirstY + AFirstX;
  RandomBase := FWidth * FHeight;
  Randomize;
  DroppedMines :=0;
  while DroppedMines < FMineCount do
  begin
    DroppedNumber := Random(RandomBase);

    XPos := DroppedNumber mod FWidth;
    YPos := DroppedNumber div FWidth;

    if (FCellState[XPos, YPos].SurroundNumber <> -1) and (DroppedNumber <> DeadNumber) then
    begin
      FCellState[XPos, YPos].SurroundNumber := -1;
      SetSurround(XPos, YPos);
      Inc(DroppedMines);
    end;
  end;
end;

procedure TMinerField.BombMarkCell(AXPos, AYPos: Integer);
var
  cState: TCellStateRec;
begin
  cState.SurroundNumber := CellState[AXPos, AYPos].SurroundNumber;
  cState.CellState := csBombMarked;
  CellState[AXPos, AYPos] := cState;
end;

procedure TMinerField.QuestionMarkCell(AXPos, AYPos: Integer);
var
  cState: TCellStateRec;
begin
  cState.SurroundNumber := CellState[AXPos, AYPos].SurroundNumber;
  cState.CellState := csQuestionMarked;
  CellState[AXPos, AYPos] := cState;
end;

procedure TMinerField.CloseCell(AXPos, AYPos: Integer);
var
  cState: TCellStateRec;
begin
  cState.SurroundNumber := CellState[AXPos, AYPos].SurroundNumber;
  cState.CellState := csClosed;
  CellState[AXPos, AYPos] := cState;
end;

procedure TMinerField.OpenSurround(AXPos, AYPos: Integer; var AChangedCells: TCells);
  procedure SetCellState(AXPos, AYPos: Integer; var AChangedCells: TCells);
  begin
    if FCellState[AXPos, AYPos].SurroundNumber <> -1 then
      FCellState[AXPos, AYPos].CellState := csOpened;
    SetLength(AChangedCells, Length(AChangedCells)+1);
    AChangedCells[High(AChangedCells)].x := AXPos;
    AChangedCells[High(AChangedCells)].y := AYPos;
  end;
  function CheckIfClosed_Surround(AXPos, AYPos: Integer): Boolean;
  begin
    Result := False;
    if (FCellState[AXPos, AYPos].CellState = csClosed)
      or (FCellState[AXPos, AYPos].CellState = csQuestionMarked) then
      if FCellState[AXPos, AYPos].SurroundNumber = 0 then
        Result := True
      else
        SetCellState(AXPos, AYPos, AChangedCells);
  end;
begin
  SetCellState(AXPos, AYPos, AChangedCells);
  if CheckFieldBounds(AXPos-1, AYPos-1) and CheckIfClosed_Surround(AXPos-1, AYPos-1) then
    OpenSurround(AXPos-1, AYPos-1, AChangedCells);
  if CheckFieldBounds(AXPos-1, AYPos) and CheckIfClosed_Surround(AXPos-1, AYPos) then
    OpenSurround(AXPos-1, AYPos, AChangedCells);
  if CheckFieldBounds(AXPos, AYPos-1) and CheckIfClosed_Surround(AXPos, AYPos-1) then
    OpenSurround(AXPos, AYPos-1, AChangedCells);
  if CheckFieldBounds(AXPos+1, AYPos+1) and CheckIfClosed_Surround(AXPos+1, AYPos+1) then
    OpenSurround(AXPos+1, AYPos+1, AChangedCells);
  if CheckFieldBounds(AXPos+1, AYPos) and CheckIfClosed_Surround(AXPos+1, AYPos) then
    OpenSurround(AXPos+1, AYPos, AChangedCells);
  if CheckFieldBounds(AXPos, AYPos+1) and CheckIfClosed_Surround(AXPos, AYPos+1) then
    OpenSurround(AXPos, AYPos+1, AChangedCells);
  if CheckFieldBounds(AXPos-1, AYPos+1) and CheckIfClosed_Surround(AXPos-1, AYPos+1) then
    OpenSurround(AXPos-1, AYPos+1, AChangedCells);
  if CheckFieldBounds(AXPos+1, AYPos-1) and CheckIfClosed_Surround(AXPos+1, AYPos-1) then
    OpenSurround(AXPos+1, AYPos-1, AChangedCells);
end;

function TMinerField.CheckFieldBounds(AXPos, AYPos: Integer): Boolean;
begin
   Result := False;
   if (AXPos >=0) and (AYPos >=0) and (AXPos <= High(FCellState)) and
   (AYPos <= High(FCellState[0])) then Result := True;
end;

function TMinerField.Get_GameStatus: TGameStatus;
begin
  Result := FGameStatus;
end;

procedure TMinerField.Set_GameStatus(const Value: TGameStatus);
begin
  FGameStatus := Value;
  if FGameStatus = gsNew then
    CreateNewGame;
end;

function TMinerField.IsGameFinished: Boolean;
var
  i, j: Integer;
  FreeCells: Integer;
begin
  Result := False;
  FreeCells := 0;
  for i:=0 to High(FCellState) do
    for j:=0 to High(FCellState[i]) do
      if (FCellState[i, j].CellState = csClosed) or
        (FCellState[i, j].CellState = csQuestionMarked) then
      begin
        Inc(FreeCells);
        if FreeCells > FMineCount then Exit;
      end;
  if FreeCells = (FMineCount - FCellsBombMarkedCount) then Result := True;
end;

procedure TMinerField.FireEvGameStatusChanged(AGameStatus: TGameStatus; var AChangedCells: TCells);
var
  i: Integer;
  ACells: TChangedCells;
  ARedCells: TCells;
begin
  if Assigned(FGameStatusChanged) then
  begin
    SetLength(ACells, Length(AChangedCells));
    for i:=0 to High(AChangedCells) do
    begin
      ACells[i].Pos := AChangedCells[i];
      ACells[i].CellState := FCellState[AChangedCells[i].x, AChangedCells[i].y];
    end;
    SetLength(ARedCells, Length(FRedCells));
    for i:=0 to High(FRedCells) do
      ARedCells[i] := FRedCells[i];
    FGameStatusChanged(Self, AGameStatus, FGameDifficulty, ACells, ARedCells);
  end;
  AChangedCells := nil;
end;

procedure TMinerField.FireEvMinerFieldChanged(var AChangedCells: TCells);
var
  ACells: TChangedCells;
  ARedCells: TCells;
  i: Integer;
begin
  if Assigned(FMinerFieldChanged) then
  begin
    SetLength(ACells, Length(AChangedCells));
    for i:=0 to High(AChangedCells) do
      with AChangedCells[i] do
      begin
        ACells[i].Pos.x := x;
        ACells[i].Pos.y := y;
        ACells[i].CellState.CellState := FCellState[x, y].CellState;
        ACells[i].CellState.SurroundNumber := FCellState[x, y].SurroundNumber;
      end;
    AChangedCells := nil;
    SetLength(ARedCells, Length(FRedCells));
    for i:=0 to High(FRedCells) do
      ARedCells[i] := FRedCells[i];

    FMinerFieldChanged(Self, ACells, ARedCells);
  end;
end;

procedure TMinerField.HandleEvCreateNewGame(Sender: TObject);
begin
  GameStatus := gsNew;
end;

procedure TMinerField.HandleEvChangeGameDifficulty(Sender: TObject; const AGameDifficulty: TGameDifficulty);
begin
  GameDifficulty := AGameDifficulty;
end;

procedure TMinerField.HandleMinerFieldActionEvent(Sender: TObject; ACol,
  ARow: Integer; AMinerFieldEventType: TMinerFieldActionEventType);
var
  AChangedCell: TCells;
begin
  SetLength(AChangedCell, 1);
  AChangedCell[High(AChangedCell)].x := ACol;
  AChangedCell[High(AChangedCell)].y := ARow;
  case AMinerFieldEventType of
    meOpenCell: OpenCell(ACol, ARow);
    meCloseCell:
    begin
      CloseCell(ACol, ARow);
      FireEvMinerFieldChanged(AChangedCell);
    end;
    meBombMarkCell:
    begin
      BombMarkCell(ACol ,ARow);
      FireEvMinerFieldChanged(AChangedCell);
    end;
    meQuestionMarkCell:
    begin
      QuestionMarkCell(ACol, ARow);
      FireEvMinerFieldChanged(AChangedCell);
    end;
    meCheckSurround: CheckSurround(ACol, ARow);
  end;
end;

initialization
  MinerField := TMinerField.Create;

finalization
  MinerField.Free;
end.
