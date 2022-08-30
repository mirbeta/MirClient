unit UnboundModeDemoIntMinerField;

{$I cxVer.inc}

interface

uses
  Variants,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, cxGrid, UnboundModeDemoMinerCore, cxGridCustomTableView, cxGraphics,
  UnboundModeDemoTypes,  UnboundModeDemoMinerDataSource, cxGridTableView,
  cxGridCustomView, cxLookAndFeels;
type
  TIntMinerField = class(TcxCustomGrid)
  private
    FColCount: Integer;
    FRowCount: Integer;
    FCellWidth: Integer;
    FGameStatus: TGameStatus;
    FRedCells: TCells;

    FSurroundColors: TColors;
    FMinerFieldDataSource: TMinerFieldDataSource;

    { Scheme colors}
    FOpenCellBkColor: TColor;
    FClosedCellBkColor: TColor;
    FFrameColor: TColor;
    FRectangleColor: TColor;

    FQuestionMarkCell: Boolean;

    FImages: TImageList;
    FPressedCells: TCells;

    FGameDifficulty: TGameDifficulty;

    FSurprised: Boolean;
    FCreateNewGameEvent: TCreateNewGameEvent;
    FOnMinerFieldAction: TMinerFieldActionEvent;
    FMineCountChanged: TMineCountChangedEvent;
    FOnImageChanged: TImageChangedEvent;
    FGameStatusChanged: TFormGameStatusChangedEvent;
    FColorScheme: TColorScheme;
    procedure SetSchemeColors;
    function GetCellState(ACol, ARow: Integer): TCellStateRec;
    procedure AddPressedCell(ACol, ARow: Integer);
    function CheckFieldBounds(AXPos, AYPos: Integer): Boolean;

    procedure InitNewGame;
    procedure UnpressAndInvalidate;

    procedure InvalidateCells(const AChangedCells: TCells; const ARedCells: TCells);
    procedure UpdateMinerFieldState(const ARedCells: TCells);

    procedure DrawCell(ACellState: TCellStateRec; ACol, ARow: Longint; ARect: TRect; ACanvas: TCanvas);
    procedure InvalidateCell(ACol, ARow: Integer);
    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);

    procedure HandleEvMinerFieldChanged(Sender: TObject; var AChangedCells: TCells; var ARedCells: TCells);
    procedure HandleEvGameStatusChanged(Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty; var AChangedCells: TCells; var ARedCells: TCells);
    property CellState[ACol, ARow: Integer]: TCellStateRec read GetCellState;
    procedure SetColorScheme(const Value: TColorScheme);
    procedure SetNumberColors;
  protected
    procedure FireMinerFieldEvent(ACol, ARow: Integer; AMinerFieldActionEventType: TMinerFieldActionEventType); virtual;
    procedure FireNewGameEvent; virtual;
    procedure FireImageChanged(AImageIndex: Integer); virtual;
    procedure FireSetMineCountEvent(AMineCountChangedEventType: TMineCountChangedEventType); virtual;
    procedure FireGameStatusChanged(Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty); virtual;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint); virtual;
    procedure CustomDrawCellHandler(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateNewGame;
    property QuestionMarkCell: Boolean read FQuestionMarkCell write FQuestionMarkCell;
    property Images: TImageList read FImages write FImages;
    property ColorScheme: TColorScheme read FColorScheme write SetColorScheme;
    property OnMinerFieldAction: TMinerFieldActionEvent read FOnMinerFieldAction write FOnMinerFieldAction;
    property OnImageChanged: TImageChangedEvent read FOnImageChanged write FOnImageChanged;
    property OnMineCountChanged: TMineCountChangedEvent read FMineCountChanged write FMineCountChanged;
    property OnGameStatusChanged: TFormGameStatusChangedEvent read FGameStatusChanged write FGameStatusChanged;
  end;


  TcxGridTableViewNoScrollBars = class(TcxGridTableView)
  protected
    function GetControllerClass: TcxCustomGridControllerClass; override;
  end;

  TcxGridTableControllerNoScrollBars = class(TcxGridTableController)
  public
    procedure InitScrollBarsParameters; override;
  end;

implementation

uses Extctrls, cxGridDBTableView, cxGridLevel, cxControls;

procedure AlignTextInCell(ACanvas: TCanvas; Rect: TRect; AStr: String; Alignment: TAlignment = taCenter);
var
  X, Y: Integer;
begin
  Y := 3;
  X := 1;
  case Alignment of
   taCenter: X := ((Rect.Right - Rect.Left) - ACanvas.TextWidth(AStr)) div 2;
   taLeftJustify: X := 1;
   taRightJustify:  X := ((Rect.Right - Rect.Left) - ACanvas.TextWidth(AStr)) -1;
  end;
  ACanvas.TextRect(Rect,Rect.Left + X, Rect.Top + Y, AStr);
end;

type
  TA = class(TcxGridSite);

{ TIntMinerField }

constructor TIntMinerField.Create(AOwner: TComponent);
var
  GridView: TcxGridTableView;
  Level: TcxGridLevel;
  procedure SetGridViewOptions;
  begin
    with GridView do
    begin
      OptionsData.Editing := False;
      OptionsData.Inserting := False;
      OptionsData.Deleting := False;
      OptionsView.GroupByBox := False;
      OptionsView.GridLines := glNone;
      OptionsView.FocusRect := False;

      OptionsSelection.CellSelect := False;
      OptionsSelection.HideSelection := False;
      OptionsSelection.InvertSelect := False;
      OptionsView.Header := False;
    end;
  end;
begin
  inherited Create(AOwner);
  LookAndFeel.NativeStyle := False;
  LookAndFeel.AssignedValues := [lfvNativeStyle];
  GridView := CreateView(TcxGridTableViewNoScrollBars) as TcxGridTableView;

  Level := Levels.Add;
  Level.GridView := GridView;

  FMinerFieldDataSource := TMinerFieldDataSource.Create;

  MinerField.OnMinerFieldChanged := FMinerFieldDataSource.HandleEvMinerFieldChanged;
  MinerField.OnGameStatusChanged := FMinerFieldDataSource.HandleEvGameStatusChanged;

  FMinerFieldDataSource.OnMinerFieldChanged := HandleEvMinerFieldChanged;
  FMinerFieldDataSource.OnGameStatusChanged := HandleEvGameStatusChanged;

  GridView.DataController.CustomDataSource := FMinerFieldDataSource;
  SetGridViewOptions;

  GridView.OnCustomDrawCell := CustomDrawCellHandler;

  GridView.OnMouseDown := MouseDownHandler;
  GridView.OnMouseUp := MouseUpHandler;
  GridView.OnMouseMove := MouseMoveHandler;

  OnMinerFieldAction := MinerField.HandleMinerFieldActionEvent;

  FCellWidth := GridView.ViewInfo.RecordsViewInfo.RowHeight;
  FColorScheme := csBlue;
  SetNumberColors;
  SetSchemeColors;
end;

destructor TIntMinerField.Destroy;
begin
  FOnMinerFieldAction := nil;
  FMineCountChanged := nil;
  FOnImageChanged := nil;
  FGameStatusChanged := nil;
  FPressedCells := nil;
  FRedCells := nil;
  FMinerFieldDataSource.Free;
  inherited Destroy;
end;

procedure TIntMinerField.CreateNewGame;
begin
  FireNewGameEvent;
  InitNewGame;
end;

procedure TIntMinerField.InitNewGame;
var
  i: Integer;
begin
  FRedCells := nil;

  FColCount := FGameDifficulty.Width;
  FRowCount := FGameDifficulty.Height;
  BeginUpdate;
  try

    Width := FColCount * FCellWidth + 2;
    Height := FRowCount * FCellWidth + 2;
    Top := psBorder + biBoardHeight - psOuterFrameWidth;
    Left := psBorder;

    i := TForm(Owner).ClientRect.Right - TForm(Owner).ClientRect.Left;
    i := TForm(Owner).Width - i - psOuterFrameWidth;
    TForm(Owner).Width :=2*psBorder + Width + i;// + psOuterFrameWidth;

    i := TForm(Owner).ClientRect.Bottom - TForm(Owner).ClientRect.Top;
    i := TForm(Owner).Height - i;
    TForm(Owner).Height := i + 2*(psBorder - psOuterFrameWidth) + biBoardHeight +
      Height;
    if Assigned(TForm(Owner).OnResize) then
      TForm(Owner).OnResize(Owner);

    if not Enabled then Enabled := True;
    for i := (Views[0] as TcxGridTableView).ColumnCount - 1 downto FColCount do
      (Views[0] as TcxGridTableView).Columns[i].Free;
    for i:=0 to FGameDifficulty.Width - 1 do
    begin
      if i >= (Views[0] as TcxGridTableView).ColumnCount then
        with (Views[0] as TcxGridTableView).CreateColumn do
        begin
          MinWidth := FCellWidth;
          Width := FCellWidth;
        end;
    end;
  finally
    EndUpdate;
  end;

end;

procedure TIntMinerField.DrawCell(ACellState: TCellStateRec; ACol, ARow: Integer; ARect: TRect; ACanvas: TCanvas);
var
  CellStr: String;
  procedure DrawOpenedCell;
  begin
    with ACanvas do
    begin
      Brush.Color := FOpenCellBkColor;
      FillRect(ARect);
      Pen.Style := psSolid;
      Pen.Color := FRectangleColor;
      Dec(ARect.Left); Dec(ARect.Top);
      Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      Font.Style := [fsBold];
      with Font do
        if ACellState.SurroundNumber >= 1 then
          Color := FSurroundColors[ACellState.SurroundNumber-1];
      Brush.Style := bsClear;
      if ACellState.SurroundNumber = 0 then
        CellStr := ''
      else
        CellStr := IntToStr(ACellState.SurroundNumber);
      AlignTextInCell(ACanvas, ARect, CellStr);
    end;
  end;
  procedure DrawClosedCell;
  begin
    if FGameStatus = gsLost then
    begin
      if IsExistsInArray(FRedCells, ACol, ARow) then
      begin
        // red bomb on an empty background
        with ACanvas do
        begin
          Brush.Color := FOpenCellBkColor;
          FillRect(ARect);

          Brush.Style := bsSolid;
          Brush.Color := clRed;
          Dec(ARect.Right);
          Dec(ARect.Bottom);
          FillRect(ARect);
          Brush.Style := bsClear;
        end;
        Inc(ARect.Left); Inc(ARect.Top);
        Inc(ARect.Right); Dec(ARect.Bottom);
        FImages.Draw(ACanvas, ARect.Left, ARect.Top, imRedBomb);
        Exit;
      end;
      if ACellState.SurroundNumber = -1 then
      begin
        with ACanvas do
        begin
          Brush.Color := FOpenCellBkColor;
          FillRect(ARect);
          Pen.Style := psSolid;
          Pen.Color := FRectangleColor;
          Dec(ARect.Left); Dec(ARect.Top);
          Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
          Inc(ARect.Left, 2); Inc(ARect.Top, 2);
          Brush.Style := bsClear;
       end;
       FImages.Draw(ACanvas, ARect.Left, ARect.Top, imBomb);  // bomb image
      end
      else
        Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1) // unpressed
    end else
    if (FGameStatus = gsNew) or (FGameStatus = gsRun) then
    begin
      if IsExistsInArray(FPressedCells, ACol, ARow) then
        Frame3D(ACanvas, ARect, FOpenCellBkColor, FFrameColor, 1) // pressed
      else
        Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1) // unpressed
    end;
    if (FGameStatus = gsWon) then
    begin
       Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
       if ACellState.SurroundNumber = -1 then
         FImages.Draw(ACanvas, ARect.Left, ARect.Top, imBombMark);  // bomb flag
    end;
  end;
  procedure DrawBombMarkedCell;
  begin
    if FGameStatus = gsLost then
    begin
      if ACellState.SurroundNumber = -1 then
      begin
        Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
        FImages.Draw(ACanvas, ARect.Left, ARect.Top, imBombMark)  // bomb flag
      end
      else
      begin
        // striked out bomb on an empty background
        with ACanvas do
        begin
          Brush.Color := FOpenCellBkColor;
          FillRect(ARect);
          Pen.Style := psSolid;
          Pen.Color := FRectangleColor;
          Dec(ARect.Left); Dec(ARect.Top);
          Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
          Inc(ARect.Left, 2); Inc(ARect.Top, 2);
          Brush.Style := bsClear;
       end;
        FImages.Draw(ACanvas, ARect.Left, ARect.Top, imStruckOutBomb)
      end;
    end else
    if (FGameStatus = gsRun) or (FGameStatus = gsWon)
      or (FGameStatus = gsNew) then
    begin
      Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
      FImages.Draw(ACanvas, ARect.Left, ARect.Top, imBombMark);  // bomb flag
    end;
  end;
  procedure DrawQuestionMarkedCell;
  begin
    if (FGameStatus = gsWon) then
    begin
      if ACellState.SurroundNumber = -1 then
      begin
        Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
        FImages.Draw(ACanvas, ARect.Left, ARect.Top, imBombMark);  // bomb flag
      end
    end else
    if (FGameStatus = gsLost)  then
    begin
      if ACellState.SurroundNumber = -1 then
      begin
        with ACanvas do
        begin
          Brush.Color := FClosedCellBkColor;
          FillRect(ARect);
          Pen.Style := psSolid;
          Pen.Color := FOpenCellBkColor;
          Dec(ARect.Left); Dec(ARect.Top);
          Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
          Inc(ARect.Left, 2); Inc(ARect.Top, 2);
          Brush.Style := bsClear;
       end;
       FImages.Draw(ACanvas, ARect.Left, ARect.Top, imBomb) // bomb on an empty background
     end else
     begin
       Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressesd
       FImages.Draw(ACanvas, ARect.Left, ARect.Top, imQuestionMark);      // question mark
     end
     end else
     if (FGameStatus = gsNew) or (FGameStatus = gsRun) then
     begin
       if not IsExistsInArray(FPressedCells, ACol, ARow) then
         Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1) // unpresses
       else
         Frame3D(ACanvas, ARect, FOpenCellBkColor, FFrameColor, 1); // pressed
       FImages.Draw(ACanvas, ARect.Left, ARect.Top, imQuestionMark);      // question mark
     end
  end;
begin
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FClosedCellBkColor;
    FillRect(ARect);
  end;

  case ACellState.CellState of
    csOpened: DrawOpenedCell;
    csClosed: DrawClosedCell;
    csBombMarked: DrawBombMarkedCell;
    csQuestionMarked: DrawQuestionMarkedCell;
  end;
end;

procedure TIntMinerField.SetSchemeColors;
begin
  FOpenCellBkColor := SchemeColors[Integer(FColorScheme), 0];
  FClosedCellBkColor := SchemeColors[Integer(FColorScheme), 1];
  FFrameColor := SchemeColors[Integer(FColorScheme), 2];
  FRectangleColor := SchemeColors[Integer(FColorScheme), 3];
end;

procedure TIntMinerField.SetNumberColors;
begin
  SetLength(FSurroundColors, 8);
  FSurroundColors[0] := clBlue;
  FSurroundColors[1] := clGreen;
  FSurroundColors[2] := clRed;
  FSurroundColors[3] := clNavy;
  FSurroundColors[4] := clPurple;
  FSurroundColors[5] := clBlue;
  FSurroundColors[6] := clBlue;
  FSurroundColors[7] := clGray;
end;

procedure TIntMinerField.AddPressedCell(ACol, ARow: Integer);
begin
  SetLength(FPressedCells, Length(FPressedCells) + 1);
  FPressedCells[High(FPressedCells)].x := ACol;
  FPressedCells[High(FPressedCells)].y := ARow;
end;

function TIntMinerField.CheckFieldBounds(AXPos, AYPos: Integer): Boolean;
begin
   Result := False;
   if (AXPos >=0) and (AYPos >=0) and (AXPos < FColCount) and
     (AYPos < FRowCount) then Result := True;
end;

procedure TIntMinerField.FireNewGameEvent;
begin
  if Assigned(FCreateNewGameEvent) then
    FCreateNewGameEvent(Self);
end;

procedure TIntMinerField.UnpressAndInvalidate;
var
  i: Integer;
  CellsToInvalidate: TCells;
begin
  SetLength(CellsToInvalidate, Length(FPressedCells));
  for i:=0 to High(FPressedCells) do
    CellsToInvalidate[i] := FPressedCells[i];
  FPressedCells := nil;
  for i:=0 to High(CellsToInvalidate) do
    InvalidateCell(CellsToInvalidate[i].x,
      CellsToInvalidate[i].y);
end;

procedure TIntMinerField.InvalidateCells(const AChangedCells: TCells; const ARedCells: TCells);
var
  i: Integer;
begin
  for i:=0 to High(AChangedCells) do
    with AChangedCells[i] do
      InvalidateCell(x, y);
  for i:=0 to High(ARedCells) do
    with ARedCells[i] do
      InvalidateCell(x, y);
end;

procedure TIntMinerField.FireMinerFieldEvent(ACol, ARow: Integer; AMinerFieldActionEventType: TMinerFieldActionEventType);
begin
  if Assigned(FOnMinerFieldAction) then
    FOnMinerFieldAction(Self, ACol, ARow, AMinerFieldActionEventType);
end;

procedure TIntMinerField.UpdateMinerFieldState(const ARedCells: TCells);
var
  i: Integer;
begin
  SetLength(FRedCells, Length(ARedCells));
  for i:=0 to High(ARedCells) do
    FRedCells[i] := ARedCells[i];
end;

procedure TIntMinerField.FireImageChanged(AImageIndex: Integer);
begin
  if Assigned(FOnImageChanged) then
    FOnImageChanged(Self, AImageIndex);
end;

procedure TIntMinerField.FireSetMineCountEvent(AMineCountChangedEventType: TMineCountChangedEventType);
begin
  if Assigned(FMineCountChanged) then
    FMineCountChanged(Self, AMineCountChangedEventType);
end;

procedure TIntMinerField.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  ACol := -1;
  ARow := -1;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest is TcxGridRecordCellHitTest then
  begin
    ACol := TcxGridRecordCellHitTest(AHitTest).Item.Index;
    ARow := TcxGridRecordCellHitTest(AHitTest).GridRecord.Index;
  end;
end;

procedure TIntMinerField.InvalidateCell(ACol, ARow: Integer);
var
  InvalidRect: TRect;
begin
  with (Views[0] as TcxGridTableView) do
    InvalidRect := ViewInfo.RecordsViewInfo[ARow].GetCellViewInfoByItem(Items[ACol]).Bounds;
  Views[0].Painter.Invalidate(InvalidRect);
end;

procedure TIntMinerField.MouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  i, j: Integer;
begin
  SetCaptureControl(Views[0].Site);
  MouseToCell(X, Y, ACol, ARow);
  if (ACol = - 1) or (ARow = -1) then
    Exit;

  if (Shift = [ssLeft, ssRight]) or (Shift = [ssLeft, ssRight])
    or (Shift = [ssMiddle]) or (Shift = [ssLeft, ssMiddle])
    or (Shift = [ssRight, ssMiddle]) then
  begin
    FireImageChanged(imAstonisment);
    FSurprised := True;
    for i:=-1 to 1 do
      for j:=-1 to 1 do
          if CheckFieldBounds(ACol+i, ARow+j) and (
            (CellState[ACol+i, ARow+j].CellState = csClosed)
            or (CellState[ACol+i, ARow+j].CellState = csQuestionMarked)) then
          begin
            AddPressedCell(ACol+i, ARow+j);
            InvalidateCell(ACol+i, ARow+j);
          end;
    Exit;
  end;

  if Button = mbLeft then
  begin
    FireImageChanged(imAstonisment);
    FSurprised := True;
    if (CellState[ACol, ARow].CellState = csClosed)
        or (CellState[ACol, ARow].CellState = csQuestionMarked) then
      begin
        AddPressedCell(ACol, ARow);
        InvalidateCell(ACol, ARow);
      end;
    Exit;
  end;

  if Button = mbRight then
  begin
    if CellState[ACol, ARow].CellState = csOpened then Exit;
    if CellState[ACol, ARow].CellState = csClosed then
    begin
      FireMinerFieldEvent(ACol, ARow, meBombMarkCell);
      FireSetMineCountEvent(mcDecMineCount);
    end
    else

    if (CellState[ACol, ARow].CellState = csBombMarked) and
      FQuestionMarkCell then
    begin
      FireMinerFieldEvent(ACol, ARow, meQuestionMarkCell);
      FireSetMineCountEvent(mcIncMineCount);
    end
    else
    begin
      FireMinerFieldEvent(ACol, ARow, meCloseCell);
      if not FQuestionMarkCell then FireSetMineCountEvent(mcIncMineCount);
    end;
    InvalidateCell(ACol, ARow);
  end;
end;

procedure TIntMinerField.MouseMoveHandler(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  i, j: Integer;
begin
  MouseToCell(X, Y, ACol, ARow);
  if (ACol = - 1) or (ARow = -1) then
  begin
    if FPressedCells <> nil then
      UnpressAndInvalidate;
    if FSurprised then
    begin
      FireImageChanged(imSmile);
      FSurprised := False;
    end;
    Exit;
  end;
  if Shift = [ssLeft]	 then
  begin
    if (CellState[ACol, ARow].CellState = csOpened) then
    begin
      if FPressedCells <> nil then
      begin
        UnpressAndInvalidate;
        InvalidateCell(ACol, ARow);
      end;
    end else
    if (CellState[ACol, ARow].CellState = csClosed)
      or (CellState[ACol, ARow].CellState = csQuestionMarked) then
      if not IsExistsInArray(FPressedCells, ACol, ARow) then
      begin
        UnpressAndInvalidate;
        AddPressedCell(ACol, ARow);
        InvalidateCell(ACol, ARow);
      end;
    Exit;
  end;

  if (Shift = [ssLeft, ssRight]) or (Shift = [ssLeft, ssRight])
    or (Shift = [ssMiddle]) or (Shift = [ssLeft, ssMiddle])
    or (Shift = [ssRight, ssMiddle]) then
  begin
    UnpressAndInvalidate;
    for i:=-1 to 1 do
      for j:=-1 to 1 do
        if CheckFieldBounds(ACol+i, ARow+j) and
          ((CellState[ACol+i, ARow+j].CellState = csClosed) or
          (CellState[ACol+i, ARow+j].CellState = csQuestionMarked)) then
          begin
            AddPressedCell(ACol+i, ARow+j);
            InvalidateCell(ACol+i, ARow+j);
          end;
   end;
end;

procedure TIntMinerField.MouseUpHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  SetCaptureControl(nil);
  MouseToCell(X, Y, ACol, ARow);
  if (ACol = - 1) and (ARow = -1) then
    Exit;
  if ((Shift = [ssLeft]) and (Button = mbRight)) or ((Shift = [ssRight]) and (Button = mbLeft))
     or (Button = mbMiddle) then
  begin
    FireImageChanged(imSmile);
    FSurprised := False;
    UnpressAndInvalidate;
    if CellState[ACol, ARow].CellState = csOpened then
      FireMinerFieldEvent(ACol, ARow, meCheckSurround);
    Exit;
  end;
  if Button = mbLeft then
  begin
    FireImageChanged(imSmile);
    FSurprised := False;
    if IsExistsInArray(FPressedCells, ACol, ARow) then
    begin
      FireMinerFieldEvent(ACol, ARow, meOpenCell);
      UnpressAndInvalidate;
    end;
    Exit;
  end;
  if Button = mbRight then
    UnpressAndInvalidate;
end;

procedure TIntMinerField.HandleEvGameStatusChanged(Sender: TObject; AGameStatus: TGameStatus;
  AGameDifficulty: TGameDifficulty; var AChangedCells: TCells; var ARedCells: TCells);
begin
  case AGameStatus of
    gsNew:
    begin
      FGameStatus := gsNew;
      FGameDifficulty := AGameDifficulty;
      InitNewGame;
    end;
    gsRun:
    begin
      FGameStatus := gsRun;
    end;
    gsLost:
    begin
      FGameStatus := gsLost;
      UpdateMinerFieldState(ARedCells);
      InvalidateCells(AChangedCells, ARedCells);
      Views[0].Painter.Invalidate;
      Enabled := False;
    end;
    gsWon:
    begin
      FGameStatus := gsWon;
      UpdateMinerFieldState(ARedCells);
      InvalidateCells(AChangedCells, ARedCells);
      Views[0].Painter.Invalidate;
      Enabled := False;
    end
  end;
  ARedCells := nil;
  AChangedCells := nil;
  FireGameStatusChanged(Sender, AGameStatus, AGameDifficulty);
end;

procedure TIntMinerField.HandleEvMinerFieldChanged(Sender: TObject; var AChangedCells: TCells; var ARedCells: TCells);
begin
  InvalidateCells(AChangedCells, ARedCells);
  AChangedCells := nil;
  ARedCells := nil;
end;

procedure TIntMinerField.CustomDrawCellHandler(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  DrawRect: TRect;
  ACol, ARow: Integer;
  CellRec: TCellStateRec;
begin
  ACol := AViewInfo.Item.Index;
  ARow := AViewInfo.RecordViewInfo.Index;
  DrawRect := AViewInfo.Bounds;
  CellRec := FMinerFieldDataSource.CellState[ACol, ARow];
  DrawCell(CellRec, ACol, ARow, DrawRect, ACanvas.Canvas);
  ADone := True;
end;

procedure TIntMinerField.FireGameStatusChanged(Sender: TObject;
  AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty);
begin
  if Assigned(FGameStatusChanged) then
    FGameStatusChanged(Sender, AGameStatus, AGameDifficulty);
end;

function TIntMinerField.GetCellState(ACol, ARow: Integer): TCellStateRec;
begin
  Result := FMinerFieldDataSource.CellState[ACol, ARow];
end;

procedure TIntMinerField.SetColorScheme(const Value: TColorScheme);
begin
  FColorScheme := Value;
  SetSchemeColors;
  Views[0].Painter.Invalidate;
end;

{ TcxGridTableViewNoScrollBars }

function TcxGridTableViewNoScrollBars.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridTableControllerNoScrollBars;
end;

{ TcxGridTableControllerNoScrollBars }

procedure TcxGridTableControllerNoScrollBars.InitScrollBarsParameters;
begin
end;

end.



