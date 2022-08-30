unit CustomDrawTableViewDemoStylesEditor;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxButtons, cxDropDownEdit, cxMRUEdit, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxButtonEdit, CheckLst,
  ExtCtrls, ComCtrls, Buttons, cxLookAndFeelPainters, cxRadioGroup,
  cxImageComboBox, DemoUtils;

type
  TCustomDrawTableViewDemoStylesEditorForm = class(TForm)
    btnClose: TcxButton;
    tvCustomDrawItems: TTreeView;
    gbEventHandlerSettings: TGroupBox;
    lbFont: TLabel;
    sbFont: TSpeedButton;
    bvSeparator: TBevel;
    lbIndicatorGlyph: TLabel;
    cbGradient: TcxComboBox;
    mruBkImage: TcxMRUEdit;
    rbBackGroundImage: TcxRadioButton;
    rbGradient: TcxRadioButton;
    rbDependsOnTheData: TcxRadioButton;
    rbDafaultDrawing: TcxRadioButton;
    pnSampleText: TPanel;
    icbIndicatorImages: TcxImageComboBox;
    FontDialog: TFontDialog;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure mruBkImagePropertiesButtonClick(Sender: TObject);
    procedure rbBackGroundImageClick(Sender: TObject);
    procedure rbGradientClick(Sender: TObject);
    procedure rbDependsOnTheDataClick(Sender: TObject);
    procedure rbDafaultDrawingClick(Sender: TObject);
    procedure tvCustomDrawItemsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mruBkImagePropertiesChange(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure cbGradientPropertiesChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure mruBkImageKeyPress(Sender: TObject; var Key: Char);
    procedure icbIndicatorImagesPropertiesEditValueChanged(
      Sender: TObject);
  private
    FUserDefinedImage: TBitMap;
    FIsUpdating: Boolean;
    procedure DisableControls(Sender: TcxCustomMaskEdit);
    procedure SetProperties(ACustomDrawingStyle: TCustomDrawingStyle);
    function GetBkImageText: String;
    procedure SetFont;
    function GetGradientColorText: String;
  end;

var
  CustomDrawTableViewDemoStylesEditorForm: TCustomDrawTableViewDemoStylesEditorForm;

implementation

uses CustomDrawTableViewDemoMain;

{$R *.dfm}

procedure TCustomDrawTableViewDemoStylesEditorForm.FormCreate(Sender: TObject);
var
  I, J: Integer;
  p: TCustomDrawItem;
  Node: TTreeNode;
begin
  Node := tvCustomDrawItems.Items[0];
  for I:=0 to 1 do
    for J:=0 to 5 do
    begin
      p := TCustomDrawItem.Create;
      p.ViewType := TViewType(I);
      p.CustomDrawArea := TCustomDrawArea(J);
      if Node.HasChildren then
        Node := Node.GetNext;
      if Node <> nil then
        Node.Data := p;
      Node := Node.GetNext;
    end;
  tvCustomDrawItems.FullExpand;
  tvCustomDrawItems.Selected :=
    tvCustomDrawItems.Items.GetFirstNode.getFirstChild;
  tvCustomDrawItems.Items.GetFirstNode.getFirstChild.Focused := True;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.mruBkImagePropertiesButtonClick(
  Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if FUserDefinedImage = nil then
    begin
      FUserDefinedImage.Free;
      FUserDefinedImage := nil;
    end;
    FUserDefinedImage := TBitmap.Create;
    try
      FUserDefinedImage.LoadFromFile(OpenDialog.FileName);
      FIsUpdating := True;
      mruBkImage.Text := 'User Defined';
      FIsUpdating := False;
      with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      begin
        CustomDrawTableViewDemoMainForm.UserDefindedBitmap[ViewType, CustomDrawArea] := FUserDefinedImage;
        CustomDrawTableViewDemoMainForm.CustomBkImage[ViewType, CustomDrawArea] := bkiUserDefined;
      end;
    finally
      FUserDefinedImage := nil;
    end;  
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.DisableControls(Sender: TcxCustomMaskEdit);
var
  I: Integer;
begin
  for I :=0 to ComponentCount - 1 do
    if (Components[i] is TcxCustomMaskEdit) and (Components[i] <> Sender) and not (Components[i] is TcxImageComboBox) then
      (Components[i] as TControl).Enabled := False;
  if Sender <> nil then
    Sender.Enabled := True;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.rbBackGroundImageClick(
  Sender: TObject);
begin
  if not (Sender as TcxRadioButton).Checked then (Sender as TcxRadioButton).Checked := True;
  DisableControls(mruBkImage);
  with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
    CustomDrawTableViewDemoMainForm.CustomDrawingStyle[ViewType, CustomDrawArea] := cdsBkImage;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.rbGradientClick(
  Sender: TObject);
begin
  if not (Sender as TcxRadioButton).Checked then (Sender as TcxRadioButton).Checked := True;
  DisableControls(cbGradient);
  with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
    CustomDrawTableViewDemoMainForm.CustomDrawingStyle[ViewType, CustomDrawArea] := cdsGradient;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.rbDependsOnTheDataClick(Sender: TObject);
begin
  if not (Sender as TcxRadioButton).Checked then (Sender as TcxRadioButton).Checked := True;
  DisableControls(nil);
  with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
    CustomDrawTableViewDemoMainForm.CustomDrawingStyle[ViewType, CustomDrawArea] := cdsDependOnData;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.rbDafaultDrawingClick(Sender: TObject);
begin
  if not (Sender as TcxRadioButton).Checked then (Sender as TcxRadioButton).Checked := True;
  DisableControls(nil);
  with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
    CustomDrawTableViewDemoMainForm.CustomDrawingStyle[ViewType, CustomDrawArea] := cdsDefaultDrawing;
end;

function TCustomDrawTableViewDemoStylesEditorForm.GetBkImageText: String;
begin
  if (tvCustomDrawItems.Selected <> nil) and (tvCustomDrawItems.Selected.Data <> nil) then
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      case CustomDrawTableViewDemoMainForm.CustomBkImage[ViewType, CustomDrawArea] of
        bkiTile: Result := 'Tile';
        bkiSky: Result := 'Sky';
        bkiEgypt: Result := 'Egypt';
        bkiMyFace: Result := 'My Face';
        bkiUserDefined: Result := 'User Defined';
      end;
end;

function TCustomDrawTableViewDemoStylesEditorForm.GetGradientColorText: String;
begin
  if (tvCustomDrawItems.Selected <> nil) and (tvCustomDrawItems.Selected.Data <> nil) then
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      case CustomDrawTableViewDemoMainForm.CustomColorScheme[ViewType, CustomDrawArea] of
        csGrey: Result := 'Grey';
        csGold: Result := 'Gold';
        csBlue: Result := 'Blue';
        csGreen: Result := 'Green';
      end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.SetFont;
begin
   with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
    pnSampleText.Font.Assign(CustomDrawTableViewDemoMainForm.Fonts[ViewType, CustomDrawArea]);
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.SetProperties(ACustomDrawingStyle: TCustomDrawingStyle);
begin
  FIsUpdating := True;
  mruBkImage.Text := GetBkImageText;
  cbGradient.Text := GetGradientColorText;
  FIsUpdating := False;
  SetFont;
  case ACustomDrawingStyle of
    cdsBkImage: rbBackGroundImageClick(rbBackGroundImage);
    cdsGradient: rbGradientClick(rbGradient);
    cdsDependOnData: rbDependsOnTheDataClick(rbDependsOnTheData);
    cdsDefaultDrawing: rbDafaultDrawingClick(rbDafaultDrawing);
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.tvCustomDrawItemsClick(
  Sender: TObject);
  procedure SetFontVisibility(AIsVisible: Boolean = False);
  begin
    lbFont.Visible := AIsVisible;
    sbFont.Visible := AIsVisible;
    pnSampleText.Visible := AIsVisible;
  end;
  procedure SetDependsOnTheDataVisibility(AIsVisible: Boolean = False);
  begin
    rbDependsOnTheData.Visible := AIsVisible;
  end;
  procedure SetIndicatorCellVisibility(AIsVisible: Boolean);
  begin
    lbIndicatorGlyph.Visible := AIsVisible;
    icbIndicatorImages.Visible := AIsVisible;
  end;
  procedure SetIndicatorImage(AViewType: TViewType);
  begin
    icbIndicatorImages.ItemIndex :=
      CustomDrawTableViewDemoMainForm.IndicatorImageIndex[AViewType];
  end;
begin
  if (tvCustomDrawItems.Selected <> nil) then
  begin
    if (tvCustomDrawItems.Selected.Data = nil) then
      gbEventHandlerSettings.Visible := False
    else
    begin
      if not gbEventHandlerSettings.Visible then
        gbEventHandlerSettings.Visible := True;
      case TCustomDrawItem(tvCustomDrawItems.Selected.Data).CustomDrawArea of
        cdaPartBackGround:
        begin
          SetDependsOnTheDataVisibility;
          SetFontVisibility;
          SetIndicatorCellVisibility(False);
        end;
        cdaColumnHeader, cdaFooterCell, cdaGroupCell:
        begin
          SetDependsOnTheDataVisibility;
          SetFontVisibility(True);
          SetIndicatorCellVisibility(False);
        end;
        cdaCell:
        begin
          SetDependsOnTheDataVisibility(True);
          SetFontVisibility(True);
          SetIndicatorCellVisibility(False);
        end;
        cdaIndicatorCell:
        begin
          SetDependsOnTheDataVisibility;
          SetFontVisibility(False);
          SetIndicatorCellVisibility(True);
          SetIndicatorImage(TCustomDrawItem(tvCustomDrawItems.Selected.Data).ViewType);
        end;
      end;
      with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
        SetProperties(CustomDrawTableViewDemoMainForm.CustomDrawingStyle[ViewType, CustomDrawArea]);
    end;
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.FormDestroy(
  Sender: TObject);
var
  I: Integer;
begin
  for I:=0 to tvCustomDrawItems.Items.Count - 1 do
    if tvCustomDrawItems.Items[I].Data <> nil then
      TCustomDrawItem(tvCustomDrawItems.Items[I].Data).Free;
  if FUserDefinedImage = nil then
  begin
    FUserDefinedImage.Free;
    FUserDefinedImage := nil;
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.FormShow(
  Sender: TObject);
begin
  if (tvCustomDrawItems.Selected <> nil) and (tvCustomDrawItems.Selected.Data <> nil) then
  begin
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      SetProperties(CustomDrawTableViewDemoMainForm.CustomDrawingStyle[ViewType, CustomDrawArea]);
    tvCustomDrawItemsClick(tvCustomDrawItems.Selected);
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.mruBkImagePropertiesChange(
  Sender: TObject);
var
  BkImage: TBkImage;
begin
  if FIsUpdating then Exit;
  if tvCustomDrawItems.Selected <> nil then
  begin
    BkImage := bkiUserDefined;
    if (Sender as TcxMRUEdit).Text = 'Tile' then
      BkImage := bkiTile else
    if (Sender as TcxMRUEdit).Text = 'Sky' then
      BkImage := bkiSky else
    if (Sender as TcxMRUEdit).Text = 'Egypt' then
      BkImage := bkiEgypt else
    if (Sender as TcxMRUEdit).Text = 'My Face' then
      BkImage := bkiMyFace;
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      CustomDrawTableViewDemoMainForm.CustomBkImage[ViewType, CustomDrawArea] := BkImage;
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.sbFontClick(
  Sender: TObject);
begin
  with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
    FontDialog.Font.Assign(CustomDrawTableViewDemoMainForm.Fonts[ViewType, CustomDrawArea]);
  if FontDialog.Execute then
  begin
    pnSampleText.Font.Assign(FontDialog.Font);
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      CustomDrawTableViewDemoMainForm.Fonts[ViewType, CustomDrawArea] := FontDialog.Font;
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.cbGradientPropertiesChange(
  Sender: TObject);
var
  ColorScheme: TColorScheme;
begin
  if FIsUpdating then Exit;
  if tvCustomDrawItems.Selected <> nil then
  begin
    ColorScheme := csGrey;
    if (Sender as TcxComboBox).Text = 'Blue' then
      ColorScheme := csBlue else
    if (Sender as TcxComboBox).Text = 'Gold' then
      ColorScheme := csGold else
    if (Sender as TcxComboBox).Text = 'Green' then
      ColorScheme := csGreen;
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      CustomDrawTableViewDemoMainForm.CustomColorScheme[ViewType, CustomDrawArea] := ColorScheme;
  end;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.btnCloseClick(
  Sender: TObject);
begin
  Close;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.mruBkImageKeyPress(
  Sender: TObject; var Key: Char);
begin
  Key := #7;
end;

procedure TCustomDrawTableViewDemoStylesEditorForm.icbIndicatorImagesPropertiesEditValueChanged(
  Sender: TObject);
begin
  if tvCustomDrawItems.Selected.Data <> nil then
    with TCustomDrawItem(tvCustomDrawItems.Selected.Data) do
      CustomDrawTableViewDemoMainForm.IndicatorImageIndex[ViewType] := TcxImageComboBox(Sender).ItemIndex;
end;

end.
