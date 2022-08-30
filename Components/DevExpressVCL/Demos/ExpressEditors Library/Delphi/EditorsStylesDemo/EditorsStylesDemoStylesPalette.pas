unit EditorsStylesDemoStylesPalette;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, EditorsStylesDemoBase, Menus, cxPropertiesStore, cxContainer, cxEdit,
  cxControls, cxTextEdit, cxMemo, cxHeader, cxSpinEdit, cxSpinButton,
  cxCheckComboBox, cxFontNameComboBox, cxMaskEdit, cxDropDownEdit,
  cxColorComboBox, cxCheckListBox, cxTrackBar, cxProgressBar, cxLabel,
  cxGroupBox, cxSplitter, cxListView, ComCtrls, ExtCtrls, StdCtrls, ImgList, cxGraphics,
  cxRadioGroup, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxImageComboBox, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxClasses, cxGridCustomView, cxGrid, cxExtEditRepositoryItems,
  cxEditRepositoryItems, cxCalc, cxCalendar, cxCheckBox, dxCheckGroupBox, cxLookAndFeels, cxLookAndFeelPainters;

type
  TValueColumnType = (vctBorderColor, vctBorderStyle, vctColor, vctTextColor, vctTextStyle);

  TEditorsStylesDemoStylesPaletteFrame = class(TEditorsStylesDemoBaseFrame)
    ilForegroundBitmaps: TImageList;
    ilCheckGlyphs: TImageList;
    cgbEditors: TdxCheckGroupBox;
    cxProgressBar: TcxProgressBar;
    cxTrackBar: TcxTrackBar;
    cxCheckListBox: TcxCheckListBox;
    cxColorComboBox: TcxColorComboBox;
    cxCheckComboBox: TcxCheckComboBox;
    cxFontNameComboBox: TcxFontNameComboBox;
    cxCoolLabel: TcxLabel;
    lbCheckListBox: TcxLabel;
    lbFontNameComboBox: TcxLabel;
    lbDateEdit: TcxLabel;
    lbTrackBar: TcxLabel;
    lbProgressBar: TcxLabel;
    lbSpinEdit: TcxLabel;
    lbCalcEdit: TcxLabel;
    lbColorComboBox: TcxLabel;
    pnlStyle: TcxGroupBox;
    Grid: TcxGrid;
    tvStyles: TcxGridTableView;
    clnStyleCategory: TcxGridColumn;
    clnStyleValueName: TcxGridColumn;
    clnStyleValue: TcxGridColumn;
    lvStyles: TcxGridLevel;
    cxEditRepository1: TcxEditRepository;
    eriBorderStyle: TcxEditRepositoryImageComboBoxItem;
    eriColor: TcxEditRepositoryColorComboBox;
    eriTextStyle: TcxEditRepositoryCheckComboBox;
    cxCalcEdit: TcxCalcEdit;
    cxDateEdit: TcxDateEdit;
    lbCheckComboBox: TcxLabel;
    cxSpinEdit: TcxSpinEdit;
    cxLabel1: TcxLabel;
    Panel: TPanel;
    rgColorPalette: TcxRadioGroup;
    rgLookAndFeel: TcxRadioGroup;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    procedure cxTrackBarPropertiesDrawThumb(Sender: TObject;
      ACanvas: TcxCanvas; const ARect: TRect);
    procedure cxTrackBarPropertiesGetThumbRect(Sender: TObject;
      var ARect: TRect);
    procedure rgLookAndFeelPropertiesChange(Sender: TObject);
    procedure rgStylesPropertiesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure clnStyleValueGetProperties(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord;
      var AProperties: TcxCustomEditProperties);
    procedure StyleValueColumnPropertiesEditValueChanged(Sender: TObject);
  private
    FBitmap: TBitmap;
    procedure AdjustCheckListGlyphs(AStyleSheetType: TcxStyleSheetType;
      ACheckListBox: TcxCheckListBox);
    procedure SetProgressBarBitmap(AStyleSheetType: TcxStyleSheetType;
      AProgressBar: TcxProgressBar);
    procedure AdjustTrackBarThumb(AStyleSheetType: TcxStyleSheetType;
      ATrackBar: TcxTrackBar; ABitmap: TBitmap);

    function GetStyleStartRecordIndex(AStyleState: TcxContainerStateItem): Integer;
    function GetStyleStateByRecordIndex(ARecordIndex: Integer): TcxContainerStateItem;

    function GetStyleValue(AStyle: TcxCustomContainerStyle;
      AValueColumnType: TValueColumnType): Variant;
    procedure SetStyleValue(AStyle: TcxCustomContainerStyle;
      AValueColumnType: TValueColumnType; Value: Variant);

    procedure InitStylesView(AStyleController: TcxEditStyleController);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeDisplayStyle(ADisplayStyle: TcxStyleSheetType); override;
    function Name: string; override;
    function BriefName: string; override;
    function Description: String; override;
    function StylesIniPath: string; override;
  end;

const
  ValueColumnTypeCount = Integer(High(TValueColumnType)) - Integer(Low(TValueColumnType)) + 1;
  StyleCategoryCount = 4;

var
  EditorsStylesDemoStylesPaletteFrame: TEditorsStylesDemoStylesPaletteFrame;

implementation

{$R *.dfm}

const
  StyleValueNames: array[TValueColumnType] of string =
    ('BorderColor', 'BorderStyle', 'Color', 'TextColor', 'TextStyle');
  StyleCategoryOrders: array[TcxContainerStateItem] of Integer = (0, 2, 1, 3);

function FontStylesToInteger(AFontStyles: TFontStyles): Integer;
var
  AFontStyle: TFontStyle;
begin
  Result := 0;
  for AFontStyle := Low(TFontStyle) to High(TFontStyle) do
    if AFontStyle in AFontStyles then
      Result := Result or (1 shl Integer(AFontStyle));
end;

function IntegerToFontStyles(Value: Integer): TFontStyles;
var
  AFontStyle: TFontStyle;
begin
  Result := [];
  for AFontStyle := Low(TFontStyle) to High(TFontStyle) do
    if Value and (1 shl Integer(AFontStyle)) <> 0 then
      Include(Result, AFontStyle);
end;

{ TEditorsStylesDemoStylesPaletteFrame }

constructor TEditorsStylesDemoStylesPaletteFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
  FBitmap.TransparentColor := clFuchsia;
  FBitmap.Transparent := True;

  cxColorComboBox.Properties.PrepareDelphiColorList(False, False);
  HintStyle := hcstNoHint;
  FDisplayStyle := shtLightBlue;
  FTempDisplayStyle := shtLightBlue;
end;

function TEditorsStylesDemoStylesPaletteFrame.Description: String;
begin
  Result := 'Style Palette Notes';
end;

function TEditorsStylesDemoStylesPaletteFrame.Name: string;
begin
  Result := 'Style Palette';
end;

function TEditorsStylesDemoStylesPaletteFrame.BriefName: string;
begin
  Result := 'Styles';
end;

function TEditorsStylesDemoStylesPaletteFrame.StylesIniPath: string;
begin
  Result := 'StylesFrmStylePalette\';
end;

procedure TEditorsStylesDemoStylesPaletteFrame.ChangeDisplayStyle(
  ADisplayStyle: TcxStyleSheetType);
begin
  inherited ChangeDisplayStyle(ADisplayStyle);
  FDisplayStyle := FTempDisplayStyle;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.AdjustTrackBarThumb(AStyleSheetType: TcxStyleSheetType; ATrackBar: TcxTrackBar; ABitmap: TBitmap);
begin
  case AStyleSheetType of
    shtWood:
      begin
        ABitmap.LoadFromFile(StylesIniPath + 'Wood.bmp');
        ATrackBar.Properties.ThumbType := cxttCustom;
      end;
    shtDeepSea:
      begin
        ABitmap.LoadFromFile(StylesIniPath + 'DeepSea.bmp');
        ATrackBar.Properties.ThumbType := cxttCustom;
      end;
  else
    ATrackBar.Properties.ThumbType := cxttRegular;
  end;
end;

function TEditorsStylesDemoStylesPaletteFrame.GetStyleStartRecordIndex(
  AStyleState: TcxContainerStateItem): Integer;
begin
  Result := StyleCategoryOrders[AStyleState] * ValueColumnTypeCount;
end;

function TEditorsStylesDemoStylesPaletteFrame.GetStyleStateByRecordIndex(
  ARecordIndex: Integer): TcxContainerStateItem;
begin
  for Result := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
    if ARecordIndex div ValueColumnTypeCount = StyleCategoryOrders[Result] then
      Break;
end;

function TEditorsStylesDemoStylesPaletteFrame.GetStyleValue(AStyle: TcxCustomContainerStyle;
  AValueColumnType: TValueColumnType): Variant;
begin
  case AValueColumnType of
    vctBorderColor:
      Result := AStyle.BorderColor;
    vctBorderStyle:
      Result := Integer(AStyle.BorderStyle);
    vctColor:
      Result := AStyle.Color;
    vctTextColor:
      Result := AStyle.TextColor;
    vctTextStyle:
      Result := FontStylesToInteger(AStyle.TextStyle);
  end;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.SetStyleValue(AStyle: TcxCustomContainerStyle;
  AValueColumnType: TValueColumnType; Value: Variant);
begin
  case AValueColumnType of
    vctBorderColor:
      AStyle.BorderColor := Value;
    vctBorderStyle:
      AStyle.BorderStyle := TcxContainerBorderStyle(Value);
    vctColor:
      AStyle.Color := Value;
    vctTextColor:
      AStyle.TextColor := Value;
    vctTextStyle:
      AStyle.TextStyle := IntegerToFontStyles(Value);
  end;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.InitStylesView(AStyleController: TcxEditStyleController);

  procedure InitStyleRecords(AStyle: TcxCustomContainerStyle);
  var
    AStartRecordIndex: Integer;
    I: TValueColumnType;
  begin
    AStartRecordIndex := GetStyleStartRecordIndex(AStyle.State);
    for I := Low(TValueColumnType) to High(TValueColumnType) do
    begin
      tvStyles.DataController.Values[AStartRecordIndex + Integer(I),
        clnStyleValue.Index] := GetStyleValue(AStyle, I);
    end;
  end;

var
  AStyleState: TcxContainerStateItem;
begin
  tvStyles.BeginUpdate;
  try
    for AStyleState := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
      InitStyleRecords(AStyleController.Styles[AStyleState]);
  finally
    tvStyles.EndUpdate;
  end;
end;

procedure SetLookAndFeel(AEditStyleController: TcxEditStyleController; AItemIndex: Integer);
var
  Kind: TcxLookAndFeelKind;
begin
  if AItemIndex < 4 then
  begin
    Kind := TcxLookAndFeelKind(AItemIndex);
    AEditStyleController.Style.LookAndFeel.Kind := Kind;
    AEditStyleController.Style.LookAndFeel.NativeStyle := False;
  end else
    AEditStyleController.Style.LookAndFeel.NativeStyle := True;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.AdjustCheckListGlyphs(
  AStyleSheetType: TcxStyleSheetType; ACheckListBox: TcxCheckListBox);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    case AStyleSheetType of
      shtDeepSea:
        ilCheckGlyphs.GetBitmap(0, ABitmap);
      shtWood:
        ilCheckGlyphs.GetBitmap(1, ABitmap);
    end;
    ACheckListBox.Glyph.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.SetProgressBarBitmap(
  AStyleSheetType: TcxStyleSheetType; AProgressBar: TcxProgressBar);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    case AStyleSheetType of
      shtRainyDay:
        ilForegroundBitmaps.GetBitmap(1, ABitmap);
      shtBrick:
        ilForegroundBitmaps.GetBitmap(0, ABitmap);
    end;
    AProgressBar.Properties.ForegroundImage := ABitmap;
  finally
    ABitmap.Free;
  end;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.cxTrackBarPropertiesDrawThumb(
  Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.Draw(ARect.Left, ARect.Top, FBitmap);
end;

procedure TEditorsStylesDemoStylesPaletteFrame.cxTrackBarPropertiesGetThumbRect(
  Sender: TObject; var ARect: TRect);
begin
  ARect := FBitmap.Canvas.ClipRect;
end;

destructor TEditorsStylesDemoStylesPaletteFrame.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.rgLookAndFeelPropertiesChange(
  Sender: TObject);
var
  AItemIndex: Integer;
  AStyleState: TcxContainerStateItem;
begin
  AItemIndex := TcxRadioGroup(Sender).ItemIndex;
  SetLookAndFeel(cxLabelStyleController, AItemIndex);
  SetLookAndFeel(cxEditStyleController, AItemIndex);

  for AStyleState := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
    cxEditStyleController.Styles[AStyleState].AssignedValues :=
      cxEditStyleController.Styles[AStyleState].AssignedValues - [svBorderStyle, svButtonStyle];

  InitStylesView(cxEditStyleController);
end;

procedure TEditorsStylesDemoStylesPaletteFrame.rgStylesPropertiesChange(
  Sender: TObject);
var
  AStyleSheetType: TcxStyleSheetType;
begin
  SendMessage(cgbEditors.Handle, WM_SETREDRAW, 0, 0);
  try
    AStyleSheetType := TcxStyleSheetType((Sender as TcxRadioGroup).ItemIndex);
    ChangeDisplayStyle(AStyleSheetType);
    SetProgressBarBitmap(AStyleSheetType, cxProgressBar);
    AdjustCheckListGlyphs(AStyleSheetType, cxCheckListBox);
    AdjustTrackBarThumb(AStyleSheetType, cxTrackBar, FBitmap);
    InitStylesView(cxEditStyleController);
  finally
    SendMessage(cgbEditors.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(cgbEditors.Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.FormCreate(Sender: TObject);

  procedure InitRecords;
  var
    I: Integer;
  begin
    tvStyles.BeginUpdate;
    try
      tvStyles.DataController.RecordCount := StyleCategoryCount * ValueColumnTypeCount;
      for I := 0 to tvStyles.DataController.RecordCount - 1 do
      begin
        tvStyles.DataController.Values[I, clnStyleCategory.Index] := I div ValueColumnTypeCount;
        tvStyles.DataController.Values[I, clnStyleValueName.Index] :=
          StyleValueNames[TValueColumnType(I mod ValueColumnTypeCount)];
      end;
    finally
      tvStyles.EndUpdate;
    end;
    clnStyleValueName.ApplyBestFit;
//    clnStyleValue.ApplyBestFit;
    clnStyleValue.Width := 150;
  end;

begin
  InitRecords;
  InitStylesView(cxEditStyleController);
  cxDateEdit.Date := Now;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.clnStyleValueGetProperties(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
var
  AValueColumnType: TValueColumnType;
begin
  AValueColumnType := TValueColumnType(ARecord.RecordIndex mod ValueColumnTypeCount);
  case AValueColumnType of
    vctBorderColor, vctColor, vctTextColor:
      AProperties := eriColor.Properties;
    vctBorderStyle:
      AProperties := eriBorderStyle.Properties;
    vctTextStyle:
      AProperties := eriTextStyle.Properties;
  end;
end;

procedure TEditorsStylesDemoStylesPaletteFrame.StyleValueColumnPropertiesEditValueChanged(
  Sender: TObject);
var
  AFocusedRecordIndex: Integer;
begin
  AFocusedRecordIndex := tvStyles.DataController.FocusedRecordIndex;
  SetStyleValue(
    cxEditStyleController.Styles[GetStyleStateByRecordIndex(AFocusedRecordIndex)],
    TValueColumnType(AFocusedRecordIndex mod ValueColumnTypeCount), TcxCustomEdit(Sender).EditValue);
  InitStylesView(cxEditStyleController);
end;

initialization
  EditorsStylesDemoFrameManager.RegisterFrameClass(TEditorsStylesDemoStylesPaletteFrame);

end.
