unit CustomDrawDemoEditor;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxButtons, cxDropDownEdit, cxMRUEdit, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxButtonEdit, CheckLst,
  ExtCtrls, ComCtrls, CustomDrawDemoConsts, Buttons,
  cxLookAndFeelPainters, cxRadioGroup, cxImageComboBox, 
  cxStyles, cxTL, cxMemo, cxCurrencyEdit, cxCheckBox, cxDBLookupComboBox,
  cxInplaceContainer, Menus, cxEditRepositoryItems, CustomDrawDemoMain,
  cxGraphics, cxCustomData;

type
  TCustomDrawDemoEditorForm = class(TForm)
    btnClose: TcxButton;
    tlCustomDrawItems: TcxTreeList;
    gbEventHandlerSettings: TGroupBox;
    cbGradient: TcxComboBox;
    mruBkImage: TcxMRUEdit;
    rbBackGroundImage: TcxRadioButton;
    rbGradient: TcxRadioButton;
    rpendsOnTheData: TcxRadioButton;
    rfaultDrawing: TcxRadioButton;
    FontDialog: TFontDialog;
    OpenDialog: TOpenDialog;
    lbFont: TLabel;
    sbFont: TSpeedButton;
    bvSeparator: TBevel;
    tlCustomDrawItemscxTreeListColumn1: TcxTreeListColumn;
    chbOwnerDrawText: TcxCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure tlCustomDrawItemsSelectionChanged(Sender: TObject);
    procedure rbRadioButtonClick(Sender: TObject);
    procedure mruBkImagePropertiesEditValueChanged(Sender: TObject);
    procedure mruBkImagePropertiesButtonClick(Sender: TObject);
    procedure cbGradientPropertiesChange(Sender: TObject);
    procedure chbOwnerDrawTextPropertiesChange(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    procedure AdjustControlsEnable;
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
  begin
    with CustomDrawDemoMainForm do
      for I := 0 to CustomDrawInfo.Count - 1 do
        with tlCustomDrawItems.Root.AddChild do
        begin
          AItem := CustomDrawInfo.GetItemByIndex(I);
          Data := AItem;
          Values[0] := CustomDrawAreaNames[AItem.DrawArea];
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
  tlCustomDrawItems.Root.getFirstChild.Focused := True;
end;

procedure TCustomDrawDemoEditorForm.tlCustomDrawItemsSelectionChanged(
  Sender: TObject);
  procedure AdjustSettings(ASelectedNode: TcxTreeListNode);
  var
    AItem: TcxItemCustomDrawInfo;
  begin
    AItem := TcxItemCustomDrawInfo(ASelectedNode.Data);
    rbBackGroundImage.Checked := AItem.DrawingStyle = cdsBkImage;
    rbGradient.Checked := AItem.DrawingStyle = cdsGradient;
    rpendsOnTheData.Checked := AItem.DrawingStyle = cdsDependsOnData;
    rfaultDrawing.Checked := AItem.DrawingStyle = cdsDefaultDrawing;
    chbOwnerDrawText.Checked := AItem.OwnerTextDraw;
    rpendsOnTheData.Visible := AItem.ItemType = itCell;
    mruBkImage.Text := BkImageResNames[AItem.BkImageType];
    cbGradient.ItemIndex := Integer(AItem.ColorScheme);
    AdjustControlsEnable;
  end;
begin
  if tlCustomDrawItems.SelectionCount > 0 then
    AdjustSettings(tlCustomDrawItems.Selections[0]);
end;

procedure TCustomDrawDemoEditorForm.AdjustControlsEnable;
begin
  mruBkImage.Enabled := rbBackGroundImage.Checked;
  cbGradient.Enabled := rbGradient.Checked;
  chbOwnerDrawText.Enabled := (GetSelectedDrawItem.ItemType in [itText, itCell]) and
    not (rfaultDrawing.Checked or rpendsOnTheData.Checked);
  lbFont.Enabled := chbOwnerDrawText.Checked and chbOwnerDrawText.Enabled;
  sbFont.Enabled := lbFont.Enabled;
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
    AdjustControlsEnable;
    CustomDrawDemoMainForm.cxDBTreeList.LayoutChanged;
  end;
end;

function TCustomDrawDemoEditorForm.GetSelectedDrawItem: TcxItemCustomDrawInfo;
begin
  Result := nil;
  if tlCustomDrawItems.SelectionCount > 0 then
    Result := TcxItemCustomDrawInfo(tlCustomDrawItems.Selections[0].Data);
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
  CustomDrawDemoMainForm.cxDBTreeList.Invalidate;
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
    CustomDrawDemoMainForm.cxDBTreeList.Invalidate;
  end;
end;

procedure TCustomDrawDemoEditorForm.cbGradientPropertiesChange(
  Sender: TObject);
begin
  GetSelectedDrawItem.ColorScheme := TColorScheme(TcxComboBox(Sender).ItemIndex);
  CustomDrawDemoMainForm.cxDBTreeList.Invalidate;
end;

procedure TCustomDrawDemoEditorForm.chbOwnerDrawTextPropertiesChange(
  Sender: TObject);
begin
  AdjustControlsEnable;
  GetSelectedDrawItem.OwnerTextDraw := chbOwnerDrawText.Checked;
  CustomDrawDemoMainForm.cxDBTreeList.LayoutChanged;
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
    CustomDrawDemoMainForm.cxDBTreeList.Invalidate;
  end;
end;

procedure TCustomDrawDemoEditorForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
