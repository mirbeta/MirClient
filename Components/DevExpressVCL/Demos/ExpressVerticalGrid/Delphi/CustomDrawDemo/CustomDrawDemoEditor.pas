unit CustomDrawDemoEditor;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxButtons, cxDropDownEdit, cxMRUEdit, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxButtonEdit, CheckLst,
  ExtCtrls, ComCtrls, CustomDrawDemoUtils, Buttons,
  cxLookAndFeelPainters, cxRadioGroup, cxImageComboBox, 
  cxStyles, cxMemo, cxCurrencyEdit, cxCheckBox, cxDBLookupComboBox,
  cxInplaceContainer, Menus, cxEditRepositoryItems, CustomDrawDemoMain,
  cxGraphics, cxCustomData, cxVGrid;

type
  TCustomDrawDemoEditorForm = class(TForm)
    btnClose: TcxButton;
    gbEventHandlerSettings: TGroupBox;
    cbGradient: TcxComboBox;
    mruBkImage: TcxMRUEdit;
    rbBackGroundImage: TcxRadioButton;
    rbGradient: TcxRadioButton;
    rbDependsOnTheData: TcxRadioButton;
    rbDefaultDrawing: TcxRadioButton;
    FontDialog: TFontDialog;
    OpenDialog: TOpenDialog;
    lbFont: TLabel;
    sbFont: TSpeedButton;
    bvSeparator: TBevel;
    chbOwnerDrawText: TcxCheckBox;
    vgCustomDrawItems: TcxVerticalGrid;
    vgDrawItemCategory: TcxCategoryRow;
    procedure FormCreate(Sender: TObject);
    procedure rbRadioButtonClick(Sender: TObject);
    procedure mruBkImagePropertiesEditValueChanged(Sender: TObject);
    procedure mruBkImagePropertiesButtonClick(Sender: TObject);
    procedure cbGradientPropertiesChange(Sender: TObject);
    procedure chbOwnerDrawTextPropertiesChange(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure vgCustomDrawItemsItemChanged(Sender: TObject;
      AOldRow: TcxCustomRow; AOldCellIndex: Integer);
    procedure vgCustomDrawItemsStylesGetCategoryStyle(Sender: TObject;
      ARow: TcxCustomRow; var AStyle: TcxStyle);
    procedure vgCustomDrawItemsDrawRowHeader(Sender: TObject;
      ACanvas: TcxCanvas; APainter: TcxvgPainter;
      AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
  private
    procedure AdjustControlsEnableAndVisible;
    function GetSelectedDrawItem: TcxItemCustomDrawInfo;
  end;

var
  CustomDrawDemoEditorForm: TCustomDrawDemoEditorForm;

implementation

uses CustomDrawDemoData, SysUtils;

{$R *.dfm}

procedure TCustomDrawDemoEditorForm.FormCreate(Sender: TObject);
  procedure FillCustomDrawItemList;
  var
    I: Integer;
    AItem: TcxItemCustomDrawInfo;
    ACategory: TcxCategoryRow;
  begin
    with CustomDrawDemoMainForm do
      for I := 0 to CustomDrawInfo.Count - 1 do
      begin
        ACategory := TcxCategoryRow(vgCustomDrawItems.AddChild(vgDrawItemCategory, TcxCategoryRow));
        AItem := CustomDrawInfo.GetItemByIndex(I);
        ACategory.Tag := Integer(AItem);
        ACategory.Properties.Caption := CustomDrawAreaNames[AItem.DrawArea];
      end;
  end;
  procedure FillBkImageTypeList;
  var
    I: TBkImage;
  begin
    for I := Low(BkImageResNames) to High(BkImageResNames) do
      if I = bkiUserDefined then
        mruBkImage.Properties.LookupItems.Add('User Defined')
      else
        mruBkImage.Properties.LookupItems.Add(BkImageResNames[I]);
  end;
  procedure FillColorSchemeList;
  var
    I: TColorScheme;
  begin
    for I := Low(ColorSchemeNames) to High(ColorSchemeNames) do
      cbGradient.Properties.Items.Add(ColorSchemeNames[I]);
  end;
begin
  FillCustomDrawItemList;
  FillBkImageTypeList;
  FillColorSchemeList;
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  vgCustomDrawItems.FocusedRow := vgCustomDrawItems.Rows[1];
end;

procedure TCustomDrawDemoEditorForm.AdjustControlsEnableAndVisible;
begin
  mruBkImage.Enabled := rbBackGroundImage.Checked;
  cbGradient.Enabled := rbGradient.Checked;
  chbOwnerDrawText.Visible := (GetSelectedDrawItem.ItemType in [itText, itCell]) and
    not (rbDefaultDrawing.Checked or rbDependsOnTheData.Checked);
  lbFont.Visible := chbOwnerDrawText.Checked and chbOwnerDrawText.Visible;
  sbFont.Visible := lbFont.Visible;
end;

procedure TCustomDrawDemoEditorForm.rbRadioButtonClick(
  Sender: TObject);
var
  AItem: TcxItemCustomDrawInfo;
begin
  TcxRadioButton(Sender).Checked := True;
  AItem := GetSelectedDrawItem;
  if AItem <> nil then
  begin
    AItem.DrawingStyle := TCustomDrawingStyle(TcxRadioButton(Sender).Tag);
    AdjustControlsEnableAndVisible;
    CustomDrawDemoMainForm.cxDBVerticalGrid.LayoutChanged;
  end;
end;

function TCustomDrawDemoEditorForm.GetSelectedDrawItem: TcxItemCustomDrawInfo;
begin
  Result := nil;
  if vgCustomDrawItems.FocusedRow <> nil then
    Result := TcxItemCustomDrawInfo(vgCustomDrawItems.FocusedRow.Tag);
end;

procedure TCustomDrawDemoEditorForm.mruBkImagePropertiesEditValueChanged(
  Sender: TObject);
  function GetBkImageTypeByName(AName: string): TBkImage;
  var
    I: TBkImage;
  begin
    Result := bkiUserDefined;
    for I := Low(BkImageResNames) to High(BkImageResNames) do
      if BkImageResNames[I] = AName then
      begin
        Result := I;
        Break;
      end;
  end;
begin
  GetSelectedDrawItem.BkImageType :=
    GetBkImageTypeByName(TcxMRUEdit(Sender).EditValue);
  CustomDrawDemoMainForm.cxDBVerticalGrid.LayoutChanged;
end;

procedure TCustomDrawDemoEditorForm.mruBkImagePropertiesButtonClick(
  Sender: TObject);
var
  ABitmap: TBitmap;
begin
  if OpenDialog.Execute then
  begin
    ABitmap := TBitmap.Create;
    ABitmap.LoadFromFile(OpenDialog.FileName);
    GetSelectedDrawItem.Bitmap := ABitmap;
    TcxCustomEdit(Sender).EditValue := 'User Defined';
    CustomDrawDemoMainForm.cxDBVerticalGrid.Invalidate;
  end;
end;

procedure TCustomDrawDemoEditorForm.cbGradientPropertiesChange(
  Sender: TObject);
begin
  GetSelectedDrawItem.ColorScheme := TColorScheme(TcxComboBox(Sender).ItemIndex);
  CustomDrawDemoMainForm.cxDBVerticalGrid.Invalidate;
end;

procedure TCustomDrawDemoEditorForm.chbOwnerDrawTextPropertiesChange(
  Sender: TObject);
begin
  AdjustControlsEnableAndVisible;
  GetSelectedDrawItem.OwnerTextDraw := chbOwnerDrawText.Checked;
  CustomDrawDemoMainForm.cxDBVerticalGrid.Invalidate;
end;

procedure TCustomDrawDemoEditorForm.sbFontClick(Sender: TObject);
var
  AFont: TFont;
begin
  if FontDialog.Execute then
  begin
    AFont := TFont.Create;
    AFont.Assign(FontDialog.Font);
    GetSelectedDrawItem.Font := AFont;
    CustomDrawDemoMainForm.cxDBVerticalGrid.Invalidate;
  end;
end;

procedure TCustomDrawDemoEditorForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCustomDrawDemoEditorForm.vgCustomDrawItemsItemChanged(
  Sender: TObject; AOldRow: TcxCustomRow; AOldCellIndex: Integer);
  procedure AdjustSettings(ASelectedRow: TcxCategoryRow);
  var
    AItem: TcxItemCustomDrawInfo;
  begin
    gbEventHandlerSettings.Visible := ASelectedRow.Tag <> 0;
    if gbEventHandlerSettings.Visible then
    begin
      AItem := TcxItemCustomDrawInfo(ASelectedRow.Tag);
      rbBackGroundImage.Checked := AItem.DrawingStyle = cdsBkImage;
      rbGradient.Checked := AItem.DrawingStyle = cdsGradient;
      rbDependsOnTheData.Checked := AItem.DrawingStyle = cdsDependsOnData;
      rbDefaultDrawing.Checked := AItem.DrawingStyle = cdsDefaultDrawing;
      chbOwnerDrawText.Checked := AItem.OwnerTextDraw;
      rbDependsOnTheData.Visible := AItem.ItemType = itCell;
      mruBkImage.Text := BkImageResNames[AItem.BkImageType];
      cbGradient.ItemIndex := Integer(AItem.ColorScheme);
      AdjustControlsEnableAndVisible;
    end;
  end;
begin
  if (vgCustomDrawItems.FocusedRow <> nil) then
      AdjustSettings(TcxCategoryRow(vgCustomDrawItems.FocusedRow))
end;

procedure TCustomDrawDemoEditorForm.vgCustomDrawItemsStylesGetCategoryStyle(
  Sender: TObject; ARow: TcxCustomRow; var AStyle: TcxStyle);
begin
   if (vgCustomDrawItems.FocusedRow = ARow) then
    AStyle := CustomDrawDemoDataDM.cxVerticalGridStyleSheetDevExpress.Styles.Selection else
   if ARow.Level > 0 then
     AStyle := CustomDrawDemoDataDM.cxVerticalGridStyleSheetDevExpress.Styles.Content;
end;

procedure TCustomDrawDemoEditorForm.vgCustomDrawItemsDrawRowHeader(
  Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter;
  AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
begin
  AHeaderViewInfo.FocusRect := Rect(0, 0, 0, 0);
end;

end.
