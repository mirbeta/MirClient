unit EditorsStylesDemoBase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxContainer, cxEdit, cxHint, StdCtrls, cxPropertiesStore, Menus,
  cxControls, cxTextEdit, cxMemo, ExtCtrls, cxButtons, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxClasses, Contnrs;

type
  TcxExtEditorType = (eetLabel, eetProgressBar, eetTrackBar, eetCheckListBox,
    eetColorComboBox, eetFontNameComboBox, eetCheckComboBox, eetTreeView,
    eetShellTreeView, eetShellComboEdit, eetSplitter, eetGroupBox,
    eetSpinButton, eetHintStyleController, eetMCListBox, eetListView,
    eetHeader, eetShellListView, eetDBColorComboBox, eetDBLabel,
    eetDBProgressBar, eetDBTrackBar, eetDBCheckListBox, eetDBCheckComboBox,
    eetDBFontNameComboBox, eetDBShellComboEdit, eetMaskEdit, eetCheckBox,
    eetComboBox, eetButton, eetImage, eetDBTextEdit, eetDBSpinEdit,
    eetDBNavigator, eetDBDateEdit, eetDBLookupComboBox, eetDBMemo, eetGrid,
    eetCalcEdit, eetDateEdit, eetTextEdit, eetRichEdit, eetZoomTrackBar,
    eetCheckGroupBox, eetToggleSwitch);

  TcxExtEditorTypes = set of TcxExtEditorType;

  TcxHintType = (hcstLightInfo, hcstLightSlideLeft, hcstBlueSlideUp,
    hcstRoundedInfo, hcstStandard, hcstNoHint);

  TcxStyleSheetType = (shtLightBlue, shtLightGray, shtWood, shtRainyDay,
    shtBrick, shtDeepSea);

  TcxFileNameChangedEvent = procedure (AFileName: string) of object;

  TWinControlAccess = class(TWinControl);

  TEditorsStylesDemoBaseFrame = class(TForm)
    cxEditStyleController: TcxEditStyleController;
    cxPropertiesStore: TcxPropertiesStore;
    cxLabelStyleController: TcxEditStyleController;
    memDescrip: TcxMemo;
    FlickerTimer: TTimer;
    pnlDescription: TPanel;
    procedure FlickerTimerTimer(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FOnFileNameChanged: TcxFileNameChangedEvent;
    FHintStyle: TcxHintType;
    FFlickeringClassName: string;
    FFrameControls: TList;
    procedure SetDisplayStyle(const Value: TcxStyleSheetType);
    function GetFlickering: Boolean;
    procedure SetFlickering(const Value: Boolean);
  protected
    FFileName: string;
    FDisplayStyle: TcxStyleSheetType;
    FTempDisplayStyle: TcxStyleSheetType;
    FStyleBackgroundColor: TColor;
    FTempFlickering: Boolean;
    procedure DoOnFileNameChanged;
    procedure ClearFrameControls;
    procedure cxSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure cxSplitterMoved(Sender: TObject);
    property FlickeringClassName: string read FFlickeringClassName
      write FFlickeringClassName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Description: String; virtual;
    function GetExtEditorTypes(AControl: TWinControl): TcxExtEditorTypes; //virtual;
    function Name: string; virtual;
    function BriefName: string; virtual;
    function StylesIniPath: string; virtual;
    procedure ChangeDisplayStyle(ADisplayStyle: TcxStyleSheetType); virtual;
    procedure OpenFile(Sender: TObject); virtual;
    function MenuOpenFileVisible: Boolean; virtual;
    procedure SaveFile(Sender: TObject); virtual;
    function MenuSaveFileVisible: Boolean; virtual;
    function GetStyleBackgroundColor: TColor; virtual;
    procedure FlickerControls(AControlClassName: string); virtual;
    function ShowControlsAboveDescription: Boolean; virtual;
    function StyleMenuVisible: Boolean; virtual;
    property HintStyle: TcxHintType read FHintStyle write FHintStyle;
    property DisplayStyle: TcxStyleSheetType read FDisplayStyle write SetDisplayStyle;
    property FileName: string read FFileName;
    property OnFileNameChanged: TcxFileNameChangedEvent read FOnFileNameChanged write FOnFileNameChanged;
    property Flickering: Boolean read GetFlickering write SetFlickering;
  end;

  TEditorsStylesDemoBaseFrameClass = class of TEditorsStylesDemoBaseFrame;

  TEditorsStylesDemoFrameManager = class
  private
    FFrameList: TComponentList;
    function GetFrame(AIndex: Integer): TEditorsStylesDemoBaseFrame;
    function GetFramesCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterFrameClass(AFrameClass: TEditorsStylesDemoBaseFrameClass);
    property FramesCount: Integer read GetFramesCount;
    property Frames[AIndex: Integer]: TEditorsStylesDemoBaseFrame read GetFrame; default;
  end;

  procedure cxGetEditorsNamesListByTypes(AEditorsNames: TStrings;
    AExtEditorTypes: TcxExtEditorTypes);

var
  EditorsStylesDemoFrameManager: TEditorsStylesDemoFrameManager;

const
  StyleSheetIniFiles: array[TcxStyleSheetType] of string = (
    'StyleLightBlue.ini',
    'StyleLightGray.ini',
    'StyleWood.ini',
    'StyleRainyDay.ini',
    'StyleBrick.ini',
    'StyleDeepSea.ini');

implementation

uses
  EditorsStylesDemoFrameControl, cxSplitter;

{$R *.dfm}

const
  AExtEditorNames: array[TcxExtEditorType] of string = ('TcxLabel',
    'TcxProgressBar', 'TcxTrackBar', 'TcxCheckListBox', 'TcxColorComboBox',
    'TcxFontNameComboBox', 'TcxCheckComboBox', 'TcxTreeView',
    'TcxShellTreeView', 'TcxShellComboEdit', 'TcxSplitter', 'TcxGroupBox',
    'TcxSpinButton', 'TcxHintStyleController', 'TcxMCListBox', 'TcxListView',
    'TcxHeader', 'TcxShellListView', 'TcxDBColorComboBox', 'TcxDBLabel',
    'TcxDBProgressBar', 'TcxDBTrackBar', 'TcxDBCheckListBox',
    'TcxDBCheckComboBox', 'TcxDBFontNameComboBox', 'TcxDBShellComboEdit', 'TcxMaskEdit', 'TcxCheckBox',
    'TcxComboBox', 'TcxButton', 'TcxImage', 'TcxDBTextEdit', 'TcxDBSpinEdit',
    'TcxDBNavigator', 'TcxDBDateEdit', 'TcxDBLookupComboBox', 'TcxDBMemo',
    'TcxGrid', 'TcxCalcEdit', 'TcxDateEdit', 'TcxTextEdit', 'TcxRichEdit',
    'TdxZoomTrackBar', 'TdxCheckGroupBox', 'TdxToggleSwitch');

procedure cxGetEditorsNamesListByTypes(AEditorsNames: TStrings;
    AExtEditorTypes: TcxExtEditorTypes);
var
  I: TcxExtEditorType;
begin
  if not Assigned(AEditorsNames) then Exit;
  AEditorsNames.Clear;
  for I := Low(AExtEditorNames) to High(AExtEditorNames) do
    if I in AExtEditorTypes then
      AEditorsNames.Add(AExtEditorNames[I]);
end;

{ TEditorsStylesDemoFrameManager }

constructor TEditorsStylesDemoFrameManager.Create;
begin
  inherited Create;
  FFrameList := TComponentList.Create;
end;

destructor TEditorsStylesDemoFrameManager.Destroy;
var
  I: Integer;
  AFrame: TEditorsStylesDemoBaseFrame;
begin
  for I := 0 to FFrameList.Count - 1 do
  begin
    AFrame := TEditorsStylesDemoBaseFrame(FFrameList[I]);
    FreeAndNil(AFrame);
  end;
  FreeAndNil(FFrameList);
  inherited Destroy;
end;

function TEditorsStylesDemoFrameManager.GetFrame(
  AIndex: Integer): TEditorsStylesDemoBaseFrame;
begin
  Result := TEditorsStylesDemoBaseFrame(FFrameList[AIndex]);
end;

function TEditorsStylesDemoFrameManager.GetFramesCount: Integer;
begin
  Result := FFrameList.Count;
end;

procedure TEditorsStylesDemoFrameManager.RegisterFrameClass(
  AFrameClass: TEditorsStylesDemoBaseFrameClass);
var
  AFrame: TEditorsStylesDemoBaseFrame;
begin
  AFrame := AFrameClass.Create(Application);
  FFrameList.Add(AFrame);
end;

{ TEditorsStylesDemoBaseFrame }

constructor TEditorsStylesDemoBaseFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := '';
  FHintStyle := hcstLightInfo;
  FDisplayStyle := shtLightBlue;
  FTempDisplayStyle := shtLightBlue;
  FFrameControls := TList.Create;
  FTempFlickering := False;
end;

destructor TEditorsStylesDemoBaseFrame.Destroy;
begin
  ClearFrameControls;
  FreeAndNil(FFrameControls);
  inherited Destroy;
end;

function TEditorsStylesDemoBaseFrame.GetExtEditorTypes(AControl: TWinControl): TcxExtEditorTypes;

  function GetcxControlType(AcxControlName: String): TcxExtEditorTypes;
  var
    I: Integer;
  begin
    Result := [];
    for I := 0 to Integer(High(AExtEditorNames)) do
      if AExtEditorNames[TcxExtEditorType(I)] = AcxControlName then
      begin
        Result := [TcxExtEditorType(I)];
        Break;
      end;
  end;

var
  I: Integer;
begin
  Result := [];
  for i:=0 to AControl.ControlCount - 1 do
  begin
    if AControl.Controls[I] is TWinControl then
      Result := Result + GetExtEditorTypes(AControl.Controls[I] as TWinControl);
    if ((AControl.Controls[I] is TcxControl) or (AControl.Controls[I] is TcxCustomButton)) and
      TControl(AControl.Controls[I]).Visible then
      Result := Result + GetcxControlType(AControl.Controls[I].ClassName);
  end;
end;

function TEditorsStylesDemoBaseFrame.Name: string;
begin
  Result := '';
end;

function TEditorsStylesDemoBaseFrame.StylesIniPath: string;
begin
  Result := '';
end;

procedure TEditorsStylesDemoBaseFrame.ChangeDisplayStyle(
  ADisplayStyle: TcxStyleSheetType);
var
  AIniFileName: string;
begin
  if (ADisplayStyle <> FTempDisplayStyle) then
  begin
    FTempDisplayStyle := ADisplayStyle;
    AIniFileName := StylesIniPath + StyleSheetIniFiles[ADisplayStyle];
    if  FileExists(AIniFileName) then
    begin
      cxPropertiesStore.Active := True;
      cxPropertiesStore.StorageName := AIniFileName;

      cxPropertiesStore.RestoreFrom;

      cxPropertiesStore.Active := False;
    end;
  end;
end;

procedure TEditorsStylesDemoBaseFrame.SetDisplayStyle(
  const Value: TcxStyleSheetType);
begin
  if (FDisplayStyle <> Value) or
    (FDisplayStyle <> FTempDisplayStyle) then
  begin
    FDisplayStyle := Value;
    ChangeDisplayStyle(FDisplayStyle)
  end;
end;

function TEditorsStylesDemoBaseFrame.MenuOpenFileVisible: Boolean;
begin
  Result := False;
end;

function TEditorsStylesDemoBaseFrame.MenuSaveFileVisible: Boolean;
begin
  Result := False;
end;

procedure TEditorsStylesDemoBaseFrame.OpenFile(Sender: TObject);
begin
end;

procedure TEditorsStylesDemoBaseFrame.SaveFile(Sender: TObject);
begin
end;

procedure TEditorsStylesDemoBaseFrame.DoOnFileNameChanged;
begin
  if Assigned(FOnFileNameChanged) then
    FOnFileNameChanged(FFileName);
end;

function TEditorsStylesDemoBaseFrame.GetStyleBackgroundColor: TColor;
begin
  Result := Color;
end;

function TEditorsStylesDemoBaseFrame.Description: String;
begin
  Result := 'Brief Description';
end;

procedure TEditorsStylesDemoBaseFrame.SetFlickering(const Value: Boolean);
  procedure CreateFrameControls(AControl: TWinControl);
  var
    I: Integer;
    AFrameControl: TcxFrameControl;
  begin
    for I := 0 to AControl.ControlCount - 1 do
    begin
      if (AControl.Controls[I].ClassName = FFlickeringClassName) then
      begin
        if (AControl.Controls[I] is TcxSplitter) then
        begin
          TcxSplitter(AControl.Controls[I]).OnMoved := cxSplitterMoved;
          TcxSplitter(AControl.Controls[I]).OnCanResize := cxSplitterCanResize;
        end;
        AFrameControl := TcxFrameControl.Create(Self);
        AFrameControl.Parent := Self;
        AFrameControl.FrameControl(AControl.Controls[I]);
        FFrameControls.Add(AFrameControl);
      end;
      if (AControl.Controls[I] is TWinControl) then
        CreateFrameControls(TWinControl(AControl.Controls[I]));
    end;
  end;

  procedure AdjustFlickeringShapes(AIsFlickerRun: Boolean);
  begin
    ClearFrameControls;
    if AIsFlickerRun then
      CreateFrameControls(Self);
  end;
begin
  if Value <> FlickerTimer.Enabled then
  begin
    AdjustFlickeringShapes(Value);
    FlickerTimer.Enabled := Value;
  end;
end;

procedure TEditorsStylesDemoBaseFrame.FlickerControls(
  AControlClassName: string);
begin
  FFlickeringClassName := AControlClassName;
  Flickering := True;
end;

procedure TEditorsStylesDemoBaseFrame.ClearFrameControls;
begin
  while FFrameControls.Count > 0 do
  begin
    TcxFrameControl(FFrameControls[0]).Free;
    FFrameControls.Delete(0);
  end;
end;

procedure TEditorsStylesDemoBaseFrame.FlickerTimerTimer(Sender: TObject);
var
 I: Integer;
begin
  for I := 0 to FFrameControls.Count - 1 do
  begin
    TcxFrameControl(FFrameControls[I]).Visible :=
      not TcxFrameControl(FFrameControls[I]).Visible;
    TcxFrameControl(FFrameControls[I]).UpdateFrameControlPos;
  end;
end;

function TEditorsStylesDemoBaseFrame.GetFlickering: Boolean;
begin
  Result := FlickerTimer.Enabled;
end;

procedure TEditorsStylesDemoBaseFrame.cxSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if (FlickeringClassName = 'TcxSplitter') and not FTempFlickering then
  begin
    FTempFlickering := Flickering;
    Flickering := False;
  end;
end;

procedure TEditorsStylesDemoBaseFrame.cxSplitterMoved(Sender: TObject);
begin
  if (FlickeringClassName = 'TcxSplitter') then
    Flickering := FTempFlickering;
  FTempFlickering := False;
  if Flickering then FlickerTimerTimer(FlickerTimer);
end;

function TEditorsStylesDemoBaseFrame.ShowControlsAboveDescription: Boolean;
begin
  Result := False;
end;

function TEditorsStylesDemoBaseFrame.StyleMenuVisible: Boolean;
begin
  Result := True;
end;

procedure TEditorsStylesDemoBaseFrame.FrameResize(Sender: TObject);
begin
  if Flickering then FlickerTimerTimer(FlickerTimer);
end;

function TEditorsStylesDemoBaseFrame.BriefName: string;
begin
  Result := 'Base';
end;

initialization
  EditorsStylesDemoFrameManager := TEditorsStylesDemoFrameManager.Create;

finalization
  FreeAndNil(EditorsStylesDemoFrameManager);
end.
