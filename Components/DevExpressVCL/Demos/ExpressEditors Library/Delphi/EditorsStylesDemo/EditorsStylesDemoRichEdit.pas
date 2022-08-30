unit EditorsStylesDemoRichEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxHeader, cxFontNameComboBox, cxMCListBox, cxControls, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCheckComboBox, cxHint,
  cxColorComboBox, ToolWin, ComCtrls, ExtCtrls, StdCtrls, ActnList, ImgList,
  cxSpinEdit, cxSpinButton, Menus, cxPropertiesStore, EditorsStylesDemoBase,
  cxGraphics, cxMemo, cxLabel, cxLookAndFeelPainters, cxButtons, cxRichEdit;

type
  TEditorsStylesDemoRichEditFrame = class(TEditorsStylesDemoBaseFrame)
    ToolbarImages: TImageList;
    ActionList: TActionList;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditFont: TAction;
    actOpen: TAction;
    actPrint: TAction;
    actBold: TAction;
    actItalic: TAction;
    actUnderline: TAction;
    actAlignLeft: TAction;
    actAlignRight: TAction;
    actAlignCenter: TAction;
    actBullets: TAction;
    RichEdit: TcxRichEdit;
    actNewFile: TAction;
    actSaveFile: TAction;
    OpenDialog: TOpenDialog;
    PrintDialog: TPrintDialog;
    SaveDialog: TSaveDialog;
    pmColorSchemes: TPopupMenu;
    miStandard: TMenuItem;
    miHTML: TMenuItem;
    miWeb: TMenuItem;
    miWebSorted: TMenuItem;
    ControlBar: TCoolBar;
    StandardToolBar: TToolBar;
    ToolButton3: TToolButton;
    OpenButton: TToolButton;
    ToolButton4: TToolButton;
    PrintButton: TToolButton;
    ToolButton5: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    ToolButton10: TToolButton;
    BoldButton: TToolButton;
    ItalicButton: TToolButton;
    UnderlineButton: TToolButton;
    ToolButton16: TToolButton;
    LeftAlign: TToolButton;
    CenterAlign: TToolButton;
    RightAlign: TToolButton;
    ToolButton20: TToolButton;
    BulletsButton: TToolButton;
    ToolBar1: TToolBar;
    btnColorSchemes: TcxButton;
    ToolButton6: TToolButton;
    cxColorComboBox: TcxColorComboBox;
    ToolButton2: TToolButton;
    fcbFontName: TcxFontNameComboBox;
    ToolButton1: TToolButton;
    meFontSize: TcxMaskEdit;
    cxSpinButton: TcxSpinButton;
    procedure actEditCutExecute(Sender: TObject);
    procedure actBoldExecute(Sender: TObject);
    procedure actItalicExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure actAlignLeftExecute(Sender: TObject);
    procedure actAlignRightExecute(Sender: TObject);
    procedure actAlignCenterExecute(Sender: TObject);
    procedure actBulletsExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actEditCutUpdate(Sender: TObject);
    procedure actEditCopyUpdate(Sender: TObject);
    procedure actEditPasteUpdate(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure RichEditSelectionChange(Sender: TObject);
    procedure meFontSizePropertiesChange(Sender: TObject);
    procedure fcbFontNamePropertiesChange(Sender: TObject);
    procedure cxColorComboBoxPropertiesChange(Sender: TObject);
    procedure ColorSchemeButtonClick(Sender: TObject);
    procedure fcbFontNamePropertiesFontPreviewButtonClick(Sender: TObject;
      ButtonType: TcxFontButtonType);
    procedure fcbFontNamePropertiesInitPopup(Sender: TObject);
    procedure actSaveFileUpdate(Sender: TObject);
    procedure RichEditPropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUpdating: Boolean;
    FChanged: Boolean;
    procedure SetFileName(AFileName: String);
    function CurrText: TTextAttributes;
  public
    constructor Create(AOwner: TComponent); override;
    function Name: string; override;
    function BriefName: string; override;
    function MenuOpenFileVisible: Boolean; override;
    function MenuSaveFileVisible: Boolean; override;
    procedure OpenFile(Sender: TObject); override;
    procedure SaveFile(Sender: TObject); override;
    function StylesIniPath: string; override;
    function GetStyleBackgroundColor: TColor; override;
    function Description: String; override;
  end;

var
  EditorsStylesDemoRichEditFrame: TEditorsStylesDemoRichEditFrame;

implementation

uses RichEdit;

{$R *.dfm}

var
  FFileName: String = 'Untitled';
  ButtonString: String = 'Color Palette: ';

type
  TcxCustomColorComboBoxPropertiesAccess = class (TcxCustomColorComboBoxProperties);

procedure TEditorsStylesDemoRichEditFrame.actEditCutExecute(
  Sender: TObject);
begin
  RichEdit.CutToClipboard;
end;

procedure TEditorsStylesDemoRichEditFrame.actBoldExecute(Sender: TObject);
begin
  if FUpdating then Exit;
  if BoldButton.Down then
    CurrText.Style := CurrText.Style + [fsBold]
  else
    CurrText.Style := CurrText.Style - [fsBold];
end;

procedure TEditorsStylesDemoRichEditFrame.actItalicExecute(Sender: TObject);
begin
  if FUpdating then Exit;
  if ItalicButton.Down then
    CurrText.Style := CurrText.Style + [fsItalic]
  else
    CurrText.Style := CurrText.Style - [fsItalic];
end;

procedure TEditorsStylesDemoRichEditFrame.actUnderlineExecute(
  Sender: TObject);
begin
  if FUpdating then Exit;
  if UnderlineButton.Down then
    CurrText.Style := CurrText.Style + [fsUnderline]
  else
    CurrText.Style := CurrText.Style - [fsUnderline];
end;

procedure TEditorsStylesDemoRichEditFrame.actAlignLeftExecute(
  Sender: TObject);
begin
  if FUpdating or (RichEdit = nil) then Exit;
  RichEdit.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TEditorsStylesDemoRichEditFrame.actAlignRightExecute(
  Sender: TObject);
begin
  if FUpdating or (RichEdit = nil) then Exit;
  RichEdit.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TEditorsStylesDemoRichEditFrame.actAlignCenterExecute(
  Sender: TObject);
begin
  if FUpdating or (RichEdit = nil) then Exit;
  RichEdit.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TEditorsStylesDemoRichEditFrame.actBulletsExecute(
  Sender: TObject);
begin
  if FUpdating or (RichEdit = nil) then Exit;
  RichEdit.Paragraph.Numbering := TNumberingStyle(BulletsButton.Down);
end;

procedure TEditorsStylesDemoRichEditFrame.actEditCopyExecute(
  Sender: TObject);
begin
  RichEdit.CopyToClipboard;
end;

procedure TEditorsStylesDemoRichEditFrame.actEditPasteExecute(
  Sender: TObject);
begin
  RichEdit.PasteFromClipboard;
end;

procedure TEditorsStylesDemoRichEditFrame.actOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FFileName := OpenDialog.FileName;
    RichEdit.Lines.LoadFromFile(FFileName);
    RichEdit.Properties.ReadOnly := ofReadOnly in OpenDialog.Options;
    DoOnFileNameChanged;
    FChanged := False;
  end;
end;

procedure TEditorsStylesDemoRichEditFrame.actPrintExecute(Sender: TObject);
begin
  if PrintDialog.Execute then
    RichEdit.Print(FFileName);
end;

procedure TEditorsStylesDemoRichEditFrame.actEditCutUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (RichEdit <> nil) and (RichEdit.SelLength > 0);
end;

procedure TEditorsStylesDemoRichEditFrame.actEditCopyUpdate(
  Sender: TObject);
begin
  TCustomAction(Sender).Enabled := actEditCut.Enabled;
end;

procedure TEditorsStylesDemoRichEditFrame.actEditPasteUpdate(
  Sender: TObject);
begin
  TCustomAction(Sender).Enabled := RichEdit.HandleAllocated and
    (RichEdit.Perform(EM_CANPASTE, 0, 0) <> 0);
end;

procedure TEditorsStylesDemoRichEditFrame.SetFileName(AFileName: String);
begin
  FFileName := AFileName;
  DoOnFileNameChanged;
  Caption := Format('%s - %s', [ExtractFileName(AFileName), Application.Title]);
end;

procedure TEditorsStylesDemoRichEditFrame.actNewFileExecute(
  Sender: TObject);
begin
  SetFileName('Untitled');
  RichEdit.Lines.Clear;
  FChanged := False;
end;

procedure TEditorsStylesDemoRichEditFrame.actSaveFileExecute(
  Sender: TObject);
begin
  if FFileName = 'Untitled' then
  begin
    if SaveDialog.Execute then
    begin
      if FileExists(SaveDialog.FileName) then
        if MessageDlg(Format('Overwrite?', [SaveDialog.FileName]),
          mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
      RichEdit.Lines.SaveToFile(SaveDialog.FileName);
      SetFileName(SaveDialog.FileName);
      FChanged := False;
    end;
  end
  else
  begin
    FChanged := False;
    RichEdit.Lines.SaveToFile(FFileName);
  end;
end;

function TEditorsStylesDemoRichEditFrame.CurrText: TTextAttributes;
begin
{  if RichEdit.SelLength > 0 then}
    Result := RichEdit.SelAttributes
{  else
    Result := RichEdit.DefAttributes;}
end;

procedure TEditorsStylesDemoRichEditFrame.RichEditSelectionChange(
  Sender: TObject);
begin
  with RichEdit.Paragraph do
  try
    FUpdating := True;
    BoldButton.Down := fsBold in CurrText.Style;
    ItalicButton.Down := fsItalic in CurrText.Style;
    UnderlineButton.Down := fsUnderline in CurrText.Style;
    BulletsButton.Down := Boolean(Numbering);
    meFontSize.Text := IntToStr(CurrText.Size);
    fcbFontName.EditValue := CurrText.Name;
    cxColorComboBox.EditValue := CurrText.Color;
    case Ord(Alignment) of
      0: LeftAlign.Down := True;
      1: RightAlign.Down := True;
      2: CenterAlign.Down := True;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TEditorsStylesDemoRichEditFrame.meFontSizePropertiesChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  if meFontSize.EditText <> '' then
    CurrText.Size := StrToInt(meFontSize.EditText)
  else
    CurrText.Size := 0;
end;

procedure TEditorsStylesDemoRichEditFrame.fcbFontNamePropertiesChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Name := TcxFontNameComboBox(Sender).Text;
end;

constructor TEditorsStylesDemoRichEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  SetFileName('lipsum.rtf');
  RichEdit.Lines.LoadFromFile('lipsum.rtf');
  CurrText.Name := string(DefFontData.Name);
  RichEditSelectionChange(Self);
  ColorSchemeButtonClick(miStandard);

  HintStyle := hcstLightInfo;
  FDisplayStyle := shtLightBlue;
  FTempDisplayStyle := shtLightBlue;
end;

procedure TEditorsStylesDemoRichEditFrame.cxColorComboBoxPropertiesChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  if cxColorComboBox.ItemIndex <> -1 then
    CurrText.Color := cxColorComboBox.EditValue;
end;

function TEditorsStylesDemoRichEditFrame.Name: string;
begin
  Result := 'Text Processing';
end;

function TEditorsStylesDemoRichEditFrame.BriefName: string;
begin
  Result := 'Text';
end;

procedure TEditorsStylesDemoRichEditFrame.OpenFile(Sender: TObject);
begin
  actOpenExecute(nil);
end;

procedure TEditorsStylesDemoRichEditFrame.SaveFile(Sender: TObject);
begin
  actSaveFileExecute(nil);
end;

function TEditorsStylesDemoRichEditFrame.MenuOpenFileVisible: Boolean;
begin
  Result := True;
end;

function TEditorsStylesDemoRichEditFrame.MenuSaveFileVisible: Boolean;
begin
  Result := True;
end;

function TEditorsStylesDemoRichEditFrame.StylesIniPath: string;
begin
  Result := 'StylesFrmRichEdit\';
end;

function TEditorsStylesDemoRichEditFrame.GetStyleBackgroundColor: TColor;
begin
  Result := RichEdit.Style.Color;
end;

function TEditorsStylesDemoRichEditFrame.Description: String;
begin
  Result := 'Text Processing Notes';
end;

procedure TEditorsStylesDemoRichEditFrame.ColorSchemeButtonClick(Sender: TObject);
var
  AColor: TColor;
  AIndex: Integer;
  S: string;
  AProperties: TcxCustomColorComboBoxPropertiesAccess;
begin
  if not TMenuItem(Sender).Checked then
  begin
    AColor := cxColorComboBox.EditValue;
    TMenuItem(Sender).Checked := True;
    case TComponent(Sender).Tag of
      0:  begin
            cxColorComboBox.Properties.PrepareDelphiColorList(False, False);
            cxColorComboBox.Properties.NamingConvention := cxncDelphi;
            btnColorSchemes.Caption := ButtonString + 'Delphi Colors';
          end;
      1: begin
           cxColorComboBox.Properties.PrepareHTML4ColorList(False, False);
           cxColorComboBox.Properties.NamingConvention := cxncHTML4;
           btnColorSchemes.Caption := ButtonString + '16 Standard Colors';
         end;
      2: begin
           cxColorComboBox.Properties.PrepareX11ColorList(False, False);
           cxColorComboBox.Properties.NamingConvention := cxncX11;
           btnColorSchemes.Caption := ButtonString + 'Web Colors';
         end;
      3: begin
           cxColorComboBox.Properties.PrepareX11OrderedColorList(False, False);
           cxColorComboBox.Properties.NamingConvention := cxncX11;
           btnColorSchemes.Caption := ButtonString + 'Web Colors By Hue';
         end;
    end;
    AProperties := TcxCustomColorComboBoxPropertiesAccess(cxColorComboBox.Properties);
    AIndex := AProperties.IndexByValue(cxColorComboBox.EditValue);
    if AIndex <> -1 then
    begin
      S := AProperties.GetDescriptionByIndex(AIndex);
      AProperties.Items[AIndex].Description := S;
    end;
    cxColorComboBox.EditValue := AColor;
  end;
end;

procedure TEditorsStylesDemoRichEditFrame.fcbFontNamePropertiesFontPreviewButtonClick(
  Sender: TObject; ButtonType: TcxFontButtonType);
begin
  CurrText.Style := fcbFontName.Properties.FontPreview.FontStyle;
  RichEditSelectionChange(nil);
end;

procedure TEditorsStylesDemoRichEditFrame.fcbFontNamePropertiesInitPopup(
  Sender: TObject);
begin
  fcbFontName.Properties.FontPreview.FontStyle := CurrText.Style;
end;

procedure TEditorsStylesDemoRichEditFrame.actSaveFileUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := FChanged;
end;

procedure TEditorsStylesDemoRichEditFrame.RichEditPropertiesChange(
  Sender: TObject);
begin
  FChanged := True;
end;

procedure TEditorsStylesDemoRichEditFrame.FormShow(Sender: TObject);
begin
  FChanged := False;
end;

initialization
  EditorsStylesDemoFrameManager.RegisterFrameClass(TEditorsStylesDemoRichEditFrame);

end.
