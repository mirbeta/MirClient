unit UnboundModeDemoMain;

interface

uses
{$IFDEF CLR}
  Types, System.ComponentModel,
{$ENDIF}
  UnboundModeDemoTypes, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ImgList, Menus, ActnList, UnboundModeDemoIntMinerField,
  ExtCtrls, StdCtrls, cxGridLevel, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxGraphics,
  DB, cxLookAndFeels;

type
  TUnboundModeDemoMainForm = class(TForm)
    mmMain: TMainMenu;
    miGame: TMenuItem;
    miNew: TMenuItem;
    sep1: TMenuItem;
    miBeginner: TMenuItem;
    miIntermediate: TMenuItem;
    miExpert: TMenuItem;
    sep2: TMenuItem;
    miMarks: TMenuItem;
    sep3: TMenuItem;
    miBestTimes: TMenuItem;
    miExit: TMenuItem;
    miAbout: TMenuItem;
    ilGame: TImageList;
    miCustom: TMenuItem;
    N1: TMenuItem;
    ilNumbers: TImageList;
    Timer: TTimer;
    ilFaces: TImageList;
    miColors: TMenuItem;
    miGold: TMenuItem;
    miGreen: TMenuItem;
    miBlue: TMenuItem;
    miSystem: TMenuItem;
    LookAndFeelController: TcxLookAndFeelController;
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miBeginnerClick(Sender: TObject);
    procedure miIntermediateClick(Sender: TObject);
    procedure miExpertClick(Sender: TObject);
    procedure miCustomClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miMarksClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure miBestTimesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miGreenClick(Sender: TObject);
    procedure miBlueClick(Sender: TObject);
    procedure miSystemClick(Sender: TObject);
    procedure miGoldClick(Sender: TObject);
  private
    FNames: array of String;
    FTimes: array of Integer;
    FGameDifficulty: TGameDifficulty;
    FImageIndex: Integer;
    FDown: Boolean;
    FMineCount: Integer;
    FTime: Integer;
    FOnChangeGameDifficulty: TChangeGameDifficultyEvent;
    function IsPointInRect(APoint: TPoint; ARect: TRect): Boolean;
    procedure SetButtonBounds(var ARect: TRect);
    procedure FireGameDifficultyChangedEvent(const ANewGameDifficulty: TGameDifficulty);
    procedure DrawMineCount;
    procedure DrawTime;
    procedure DrawButton;
    procedure DrawIndicatorBoard;
    procedure DrawOuterFrame;
    procedure ReadMinerSettings;
    procedure WriteMinerSettings;
    procedure InitGameSettings;
    procedure ResetFastestTimes;
    procedure CheckBestTimes;
    procedure CheckMenuItem(AGameDifficulty: TDifficultyType);
  public
    IntMinerField: TIntMinerField;
    FMouseButtonPressed: Boolean;
    procedure HandleMineCountChangedEvent(Sender: TObject; AMineCountChangedEventType: TMineCountChangedEventType);
    procedure HandleEvGameStatusChanged(Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty: TGameDifficulty);
    procedure HandleEvImageChanged(Sender: TObject; AImageIndex: Integer);
  end;

var
  UnboundModeDemoMainForm: TUnboundModeDemoMainForm;

implementation

{$R *.DFM}

uses
  Registry, UnboundModeDemoMinerCore, UnboundModeDemoMinerDataSource, UnboundModeDemoCustomField,
  UnboundModeDemoFastestSweepers, AboutDemoForm;

procedure TUnboundModeDemoMainForm.FireGameDifficultyChangedEvent(const ANewGameDifficulty: TGameDifficulty);
begin
  if Assigned(FOnChangeGameDifficulty) then
    FOnChangeGameDifficulty(Self, ANewGameDifficulty);
end;

procedure TUnboundModeDemoMainForm.FormCreate(Sender: TObject);
begin
  FOnChangeGameDifficulty := MinerField.HandleEvChangeGameDifficulty;
  IntMinerField := TIntMinerField.Create(Self);
  IntMinerField.Parent := Self;
  IntMinerField.Visible := True;

  FImageIndex := imSmile;
  IntMinerField.Images := ilGame;
  IntMinerField.OnImageChanged := HandleEvImageChanged;
  IntMinerField.OnMineCountChanged := HandleMineCountChangedEvent;
  IntMinerField.OnGameStatusChanged := HandleEvGameStatusChanged;
  InitGameSettings;
  ReadMinerSettings;
end;

procedure TUnboundModeDemoMainForm.FormShow(Sender: TObject);
begin
  miNewClick(nil);
end;

procedure TUnboundModeDemoMainForm.FormDestroy(Sender: TObject);
begin
  WriteMinerSettings;
  FTimes := nil;
  FNames := nil;
end;

procedure TUnboundModeDemoMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TUnboundModeDemoMainForm.miNewClick(Sender: TObject);
begin
  FTime := 0;
  Timer.Enabled := False;
  FireGameDifficultyChangedEvent(FGameDifficulty);
  FMineCount := FGameDifficulty.MineCount;
  DrawMineCount;
end;

procedure TUnboundModeDemoMainForm.miBeginnerClick(Sender: TObject);
begin
  FGameDifficulty.DifficultyType := dtBeginner;
  miNewClick(nil);
end;

procedure TUnboundModeDemoMainForm.miIntermediateClick(Sender: TObject);
begin
  FGameDifficulty.DifficultyType := dtIntermediate;
  miNewClick(nil);
end;

procedure TUnboundModeDemoMainForm.miExpertClick(Sender: TObject);
begin
  FGameDifficulty.DifficultyType := dtExpert;
  miNewClick(nil);
end;

procedure TUnboundModeDemoMainForm.miCustomClick(Sender: TObject);
var
  CustomField: TUnboundModeDemoCustomFieldForm;
begin
  CustomField := TUnboundModeDemoCustomFieldForm.Create(Self);
  try
    CustomField.edtHeight.Text := IntToStr(FGameDifficulty.Height);
    CustomField.edtWidth.Text := IntToStr(FGameDifficulty.Width);
    CustomField.edtMineCount.Text := IntToStr(FGameDifficulty.MineCount);

    if CustomField.ShowModal = mrOK then
    with FGameDifficulty do
    begin
      Height := StrToInt(CustomField.edtHeight.Text);
      Width := StrToInt(CustomField.edtWidth.Text);
      MineCount := StrToInt(CustomField.edtMineCount.Text);
      FGameDifficulty.DifficultyType := dtCustom;
      miNewClick(nil);
    end;
  finally
    CustomField.Free;
  end;
end;

procedure TUnboundModeDemoMainForm.miBestTimesClick(Sender: TObject);

begin
  with TUnboundModeDemoFastestSweepersForm.Create(Self) do
  try
    lbBeginnerTime.Caption := IntToStr(FTimes[0]);
    lbIntermediateTime.Caption := IntToStr(FTimes[1]);
    lbExpertTime.Caption := IntToStr(FTimes[2]);
    lbBeginnerName.Caption := FNames[0];
    lbIntermediateName.Caption := FNames[1];
    ibExpertName.Caption := FNames[2];
    ShowModal;
    if FastestTimesResetted then
      ResetFastestTimes;
  finally
    Free;
  end;
end;

procedure TUnboundModeDemoMainForm.FormResize(Sender: TObject);
begin
  with Screen do
    if Left + Self.Width > Width then
      Left := Width - Self.Width;
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlueSky;
    FillRect(Rect(0, 0, Width, Height));
  end;
  FormPaint(Sender);
end;

procedure TUnboundModeDemoMainForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TUnboundModeDemoMainForm.miMarksClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    Checked := not Checked;
  IntMinerField.QuestionMarkCell := (Sender as TMenuItem).Checked;
end;

procedure TUnboundModeDemoMainForm.DrawButton;
var
  Rct, RctBk, RctPressed: TRect;
begin
  SetButtonBounds(Rct);
  with Canvas, Rct do
  begin
    Brush.Style := bsSolid;
    RctBk.Left := Left - 1;
    RctBk.Top := Top - 1;
    RctBk.Right := Right + 1;
    RctBk.Bottom := Bottom + 1;
    Brush.Color := SchemeColors[Integer(IntMinerField.ColorScheme), cliButtonColor];
    FillRect(Rect(RctBk.Left, RctBk.Top, RctBk.Right + 1, RctBk.Bottom + 1));

    Brush.Color :=
      SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dTopColor];
    FrameRect(RctBk);

    if not FMouseButtonPressed then
    begin
      Frame3d(Canvas, Rct,
        SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dBottomColor],
        SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dTopColor],  2);
      ilFaces.Draw(Canvas, Rct.Left + 1, Rct.Top + 1 , FImageIndex);
    end else
    with RctBk, Canvas do
    begin
      RctPressed.Left := Left + 1;
      RctPressed.Top := Top + 1;
      RctPressed.Right := Right + 1;
      RctPressed.Bottom := Bottom + 1;

      FrameRect(RctPressed);
      Brush.Color := SchemeColors[Integer(IntMinerField.ColorScheme), 8];
      FillRect(
      Rect(RctPressed.Left + 1, RctPressed.Top + 1, RctPressed.Right - 1, RctPressed.Bottom-1));
      ilFaces.Draw(Canvas, Rct.Left + 4, Rct.Top + 4 ,2);
    end;
  end;
end;

procedure TUnboundModeDemoMainForm.DrawOuterFrame;
var
  Rct: TRect;
begin
  Rct := Rect(1, 1, Width, Height);
  Frame3d(Canvas, Rct,
    SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dTopColor],
    SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dBottomColor],
    psOuterFrameWidth);
end;

procedure TUnboundModeDemoMainForm.DrawIndicatorBoard;
var
  Rct: TRect;
begin
  Rct := Rect(psBorder, psBorder,
    (ClientWidth - (psBorder - psOuterFrameWidth)) , biBoardHeight);
  Frame3d(Canvas, Rct,
    SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dTopColor],
    SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dBottomColor], 2);
end;

procedure TUnboundModeDemoMainForm.FormPaint(Sender: TObject);
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color :=
      SchemeColors[Integer(IntMinerField.ColorScheme), cliBackground];
    FillRect(Rect(0, 0, Width, Height));
  end;
  DrawOuterFrame;
  DrawIndicatorBoard;
  DrawTime;
  DrawMineCount;
  DrawButton;
end;

function TUnboundModeDemoMainForm.IsPointInRect(APoint: TPoint; ARect: TRect): Boolean;
begin
  Result := (ARect.Left <= APoint.x) and (APoint.x <= ARect.Right) and
            (ARect.Top <= APoint.y) and (APoint.y <= ARect.Bottom);
end;

procedure TUnboundModeDemoMainForm.SetButtonBounds(var ARect: TRect);
var
  AButtonXPos: Integer;
begin
  AButtonXPos := (Width div 2) - (biButtonWidth div 2);
  ARect := Rect(AButtonXPos, psBoardInnerIndent,
    AButtonXPos + biButtonWidth , psBoardInnerIndent + biButtonWidth);
end;

procedure TUnboundModeDemoMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Rct: TRect;
begin
  FDown := False;
  if FImageIndex = 0 then
  begin
    FImageIndex := 2;
    DrawButton;
  end;

  SetButtonBounds(Rct);
  if IsPointInRect(Point(X, Y), Rct) then
    if FMouseButtonPressed then
    begin
      FMouseButtonPressed := False;
      miNewClick(nil);
    end;
end;

procedure TUnboundModeDemoMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Rct: TRect;
begin
  SetButtonBounds(Rct);
  if FMouseButtonPressed then
  begin
    if not IsPointInRect(Point(X, Y), Rct) then
    begin
      FMouseButtonPressed := False;
      DrawButton;
    end;
  end else
  begin
    if (IsPointInRect(Point(X, Y), Rct)) and (Shift = [ssLeft])
      and FDown then
    begin
      FMouseButtonPressed := True;
      DrawButton;
    end;
  end;
end;

procedure TUnboundModeDemoMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Rct: TRect;
begin
  if Button <> mbLeft then Exit;
  SetButtonBounds(Rct);
  if not IsPointInRect(Point(X, Y), Rct) then
  begin
    if FImageIndex = 2 then
    begin
      FImageIndex := 0;
      DrawButton;
    end;
    Exit;
  end;
  if not FMouseButtonPressed then
  begin
    FMouseButtonPressed := True;
    FDown := True;
    DrawButton;
  end;
end;

procedure TUnboundModeDemoMainForm.TimerTimer(Sender: TObject);
begin
  if FTime < 999 then Inc(FTime);
  DrawTime;
end;

procedure TUnboundModeDemoMainForm.DrawMineCount;
var
  Rct: TRect;
  mCount: TArrInteger;
  I: Integer;
begin
  if FMineCount >=0 then
    MakeArrayFromInt(FMineCount, mCount, biMineDigitCount)
  else begin
    MakeArrayFromInt(Abs(FMineCount), mCount, biMineDigitCount);
    mCount[biMineDigitCount - 1] := 10; // minus
  end;
  with ilNumbers do
  begin
    Rct := Rect(psBoardInnerIndent, psBoardInnerIndent,
      psBoardInnerIndent + biMineDigitCount * Width + 2*biCountersBorderWidth,
      biNumberHeight + 2*biCountersBorderWidth);
    Frame3d(Canvas, Rct,
      SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dTopColor],
      SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dBottomColor],
      biCountersBorderWidth);
    for I := 0 to biMineDigitCount - 1 do
      Draw(Canvas, Rct.Left + Width*I, Rct.Top, mCount[biMineDigitCount - 1 - I]);
  end;
end;

procedure TUnboundModeDemoMainForm.DrawTime;
var
  Rct: TRect;
  tArr: TArrInteger;
  I, ATimerWidth: Integer;
begin
  MakeArrayFromInt(FTime, tArr, biTimerDigitCount);
  with ilNumbers do
  begin
    ATimerWidth := biTimerDigitCount * Width + 2 * biCountersBorderWidth;
    Rct := Rect(ClientWidth - ATimerWidth - psBoardInnerIndent,
      psBoardInnerIndent, ClientWidth - psBoardInnerIndent,
      biNumberHeight + 2 * biCountersBorderWidth);
    Frame3d(Canvas, Rct,
      SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dTopColor],
      SchemeColors[Integer(IntMinerField.ColorScheme), cliFrame3dBottomColor],
      biCountersBorderWidth);
    for I := 0 to biTimerDigitCount - 1 do
      Draw(Canvas, Rct.Left + Width*I, Rct.Top, tArr[biTimerDigitCount - 1 - I]);
  end;
end;

procedure TUnboundModeDemoMainForm.HandleEvGameStatusChanged(Sender: TObject; AGameStatus: TGameStatus; AGameDifficulty:
    TGameDifficulty);
begin
  case AGameStatus of
    gsNew:
    begin
      FGameDifficulty := AGameDifficulty;
      FImageIndex := 2;
      FTime := 0;
      Timer.Enabled := False;
      FMineCount := FGameDifficulty.MineCount;
      CheckMenuItem(FGameDifficulty.DifficultyType);
      OnPaint(Self);
    end;
    gsRun:
    begin
      // Timer on
      Timer.Enabled := True;
      TimerTimer(Self);
    end;
    gsLost:
    begin
      FImageIndex := 1;
      OnPaint(Self);
      // Timer off
      Timer.Enabled := False;
    end;
    gsWon:
    begin
      Timer.Enabled := False;
      FImageIndex := 3;
      FMineCount := 0;
      OnPaint(Self);
      CheckBestTimes;
    end
  end;
end;

procedure TUnboundModeDemoMainForm.ReadMinerSettings;
var
  int: Integer;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKey(Section, False);
    if ValueExists(Difficulty) then
    begin
      int := ReadInteger(Difficulty);
      case int of
        0..3: FGameDifficulty.DifficultyType := TDifficultyType(int);
      else
        FGameDifficulty.DifficultyType := dtBeginner;
      end;
    end;
    if FGameDifficulty.DifficultyType = dtCustom then
    begin
      if ValueExists('Width') then
        FGameDifficulty.Width := ReadInteger(UnboundModeDemoTypes.Width);
      if ValueExists('Height') then
        FGameDifficulty.Height := ReadInteger(UnboundModeDemoTypes.Height);
      if ValueExists('MineCount') then
        FGameDifficulty.MineCount := ReadInteger(MineCount);
    end;
    if ValueExists(Mark) then
      ReadInteger(Mark);
    if ValueExists(Name1) then
      FNames[0] := ReadString(Name1);
    if ValueExists(Name2) then
      FNames[1] := ReadString(Name2);
    if ValueExists(Name3) then
      FNames[2] := ReadString(Name3);
    if ValueExists(Time1) then
      FTimes[0] := ReadInteger(Time1);
    if ValueExists(Time2) then
      FTimes[1] := ReadInteger(Time2);
    if ValueExists(Time3) then
      FTimes[2] := ReadInteger(Time3);
  finally
    CloseKey;
    Free;
  end;
end;

procedure TUnboundModeDemoMainForm.WriteMinerSettings;
var
  i: Integer;
begin
  with TRegistry.Create do
    try
      RootKey := HKey_CURRENT_USER;
      if not OpenKey(Section, False) then
      begin
        CreateKey(Section);
        OpenKey(Section, False);
      end;
      WriteInteger(Difficulty, Integer(FGameDifficulty.DifficultyType));
      WriteInteger('Width', FGameDifficulty.Width);
      WriteInteger('Height', FGameDifficulty.Height);
      WriteInteger('MineCount', FGameDifficulty.MineCount);
      WriteInteger(Mark, 1);
      for i:=0 to High(FNames) do
      begin
        WriteString('Name' + IntToStr(i+1), FNames[i]);
        WriteInteger('Time' + IntToStr(i+1), FTimes[i]);
      end;
  finally
    CloseKey;
    Free;
  end;
end;

procedure TUnboundModeDemoMainForm.InitGameSettings;
begin
  FGameDifficulty.DifficultyType := dtBeginner;
  ResetFastestTimes;
end;

procedure TUnboundModeDemoMainForm.ResetFastestTimes;
var
  i: Integer;
begin
  SetLength(FTimes, 3);
  SetLength(FNames, 3);
  for i:=0 to High(FTimes) do
  begin
    FTimes[i] := 999;
    FNames[i] := 'Anonymous';
  end;
end;

procedure TUnboundModeDemoMainForm.CheckBestTimes;
var
  Level: String;
begin
  if FGameDifficulty.DifficultyType = dtCustom then Exit;
  if FTimes[Integer(FGameDifficulty.DifficultyType)] > FTime then
  begin
    case FGameDifficulty.DifficultyType of
      dtBeginner: Level := 'beginner';
      dtIntermediate: Level := 'intermediate';
      dtExpert: Level := 'expert';
    end;
    FTimes[Integer(FGameDifficulty.DifficultyType)] := FTime;
    FNames[Integer(FGameDifficulty.DifficultyType)] := InputBox('You are the champion in the '+ Level+' level', 'Please enter your name.',
      FNames[Integer(FGameDifficulty.DifficultyType)]);
    miBestTimesClick(nil);
  end;
end;

procedure TUnboundModeDemoMainForm.CheckMenuItem(AGameDifficulty: TDifficultyType);
begin
  case AGameDifficulty of
    dtBeginner: miBeginner.Checked := True;
    dtIntermediate: miIntermediate.Checked := True;
    dtExpert: miExpert.Checked := True;
    dtCustom: miCustom.Checked := True;
  end;
end;

procedure TUnboundModeDemoMainForm.HandleEvImageChanged(Sender: TObject; AImageIndex: Integer);
begin
  case AImageIndex of
    imSmile: FImageIndex := 2;
    imAstonisment: FImageIndex := 0;
    imWon: FImageIndex := 3;
    imLost: FImageIndex := 1;
  end;
  DrawButton;
end;

procedure TUnboundModeDemoMainForm.HandleMineCountChangedEvent(Sender: TObject;
  AMineCountChangedEventType: TMineCountChangedEventType);
begin
  case AMineCountChangedEventType of
    mcIncMineCount: Inc(FMineCount);
    mcDecMineCount: Dec(FMineCount);
  end;
  DrawMineCount;
end;

procedure TUnboundModeDemoMainForm.miGreenClick(
  Sender: TObject);
begin
  if IntMinerField.ColorScheme <> csGreen then
  begin
    IntMinerField.ColorScheme := csGreen;
    FormPaint(Self);
    TMenuItem(Sender).Checked := True;
  end;
end;

procedure TUnboundModeDemoMainForm.miBlueClick(
  Sender: TObject);
begin
  if IntMinerField.ColorScheme <> csBlue then
  begin
    IntMinerField.ColorScheme := csBlue;
    FormPaint(Self);
    TMenuItem(Sender).Checked := True;
  end
end;

procedure TUnboundModeDemoMainForm.miSystemClick(
  Sender: TObject);
begin
  if IntMinerField.ColorScheme <> csSystem then
  begin
    IntMinerField.ColorScheme := csSystem;
    FormPaint(Self);
    TMenuItem(Sender).Checked := True;
  end;
end;

procedure TUnboundModeDemoMainForm.miGoldClick(
  Sender: TObject);
begin
  if IntMinerField.ColorScheme <> csGold then
  begin
    IntMinerField.ColorScheme := csGold;
    FormPaint(Self);
    TMenuItem(Sender).Checked := True;
  end;
end;

end.  
