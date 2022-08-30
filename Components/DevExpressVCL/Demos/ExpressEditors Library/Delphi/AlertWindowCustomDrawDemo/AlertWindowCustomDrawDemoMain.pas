unit AlertWindowCustomDrawDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseForm, Menus, StdCtrls, cxLookAndFeelPainters, cxGraphics,
  dxAlertWindow, ComCtrls, ExtCtrls, cxLookAndFeels, cxButtons, ImgList,
  cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxGeometry,
  cxRichEdit, cxDropDownEdit, cxColorComboBox, dxBevel, ToolWin, cxMemo,
  cxFontNameComboBox, Math, RichEdit, dxDrawRichTextUtils;

const
  RTFFilter = 'Rich Text Files (*.RTF)|*.RTF';
  DefaultFileName = 'Document1.rtf';

type
  TfmAlertWindowCustomDraw = class(TfmBaseForm)
    awmCustomDrawDemo1: TdxAlertWindowManager;
    fdMessageText: TFontDialog;
    imglMessages: TcxImageList;
    redtMessageText: TcxRichEdit;
    tlbTextAttribute: TToolBar;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    btnSeparator1: TToolButton;
    btnAlignLeft: TToolButton;
    btnAlignCenter: TToolButton;
    btnAlignRight: TToolButton;
    btnSeparator2: TToolButton;
    btnFont: TToolButton;
    bvlLeft: TdxBevel;
    bvlTop: TdxBevel;
    bvlRight: TdxBevel;
    tlbFont: TToolBar;
    cbbFontName: TcxFontNameComboBox;
    cbTextSize: TcxComboBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderline: TToolButton;
    ilHotImages: TImageList;
    Open1: TMenuItem;
    Save1: TMenuItem;
    ccbTextColor: TcxColorComboBox;
    Panel1: TPanel;
    btnShow: TcxButton;
    procedure btnShowClick(Sender: TObject);
    procedure awmCustomDrawDemo1CustomDrawMessageText(Sender: TObject;
      AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas;
      AViewInfo: TdxAlertWindowMessageTextViewInfo; var ADone: Boolean);
    procedure awmCustomDrawDemo1MeasureMessageText(Sender: TObject;
      AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer);
    procedure btnAlignLeftClick(Sender: TObject);
    procedure btnAlignCenterClick(Sender: TObject);
    procedure btnAlignRightClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure fdMessageTextApply(Sender: TObject; Wnd: HWND);
    procedure redtMessageTextPropertiesSelectionChange(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnItalicClick(Sender: TObject);
    procedure btnUnderlineClick(Sender: TObject);
    procedure cbbFontNamePropertiesChange(Sender: TObject);
    procedure cbbFontNamePropertiesFontPreviewButtonClick(Sender: TObject;
      ButtonType: TcxFontButtonType);
    procedure FormShow(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure cbTextSizePropertiesChange(Sender: TObject);
    procedure ccbTextColorPropertiesChange(Sender: TObject);
    procedure awmCustomDrawDemo1BeforeShow(Sender: TObject;
      AAlertWindow: TdxAlertWindow);
    procedure redtMessageTextPropertiesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DrawTransparentRichEdit(ACanvas: TcxCanvas; const ARect: TRect; ARichEdit: TcxRichEdit; AMinCharIndex, AMaxCharIndex: Integer);
    procedure GetRichEditVisibleRange(ARichHandle: HWND; out AMinCharIndex, AMaxCharIndex: Integer);
    procedure GetSelAttributes;

    procedure RedrawAlertWindows;
    procedure SetSelAttributes;
  end;

var
  fmAlertWindowCustomDraw: TfmAlertWindowCustomDraw;

implementation

{$R *.dfm}

procedure TfmAlertWindowCustomDraw.GetRichEditVisibleRange(
  ARichHandle: HWND; out AMinCharIndex, AMaxCharIndex: Integer);
var
  ARect: TRect;
begin
   SendMessage(ARichHandle, EM_GETRECT, 0, LongInt(@ARect));
   AMinCharIndex := SendMessage(ARichHandle, EM_CHARFROMPOS, 0, LongInt(@(ARect.TopLeft)));
   AMaxCharIndex := SendMessage(ARichHandle, EM_CHARFROMPOS, 0, LongInt(@(ARect.Right)));
end;

procedure TfmAlertWindowCustomDraw.DrawTransparentRichEdit(ACanvas: TcxCanvas;
  const ARect: TRect; ARichEdit: TcxRichEdit; AMinCharIndex, AMaxCharIndex: Integer);
var
  AHeight: Integer;
  AStoreWindowLong: Integer;
  ARich: TRichEdit;
begin
  ARich := ARichEdit.InnerControl as TRichEdit;
  AStoreWindowLong := GetWindowLong(ARich.Handle, GWL_EXSTYLE);
  SetWindowLong(ARich.Handle, GWL_EXSTYLE, AStoreWindowLong or WS_EX_TRANSPARENT);
  try
    dxDrawRichEdit(ACanvas.Canvas, cxRectContent(ARect, ARichEdit.ContentParams.Offsets),
      ARich, AMinCharIndex, AMaxCharIndex, False, AHeight);
  finally
    SetWindowLong(ARich.Handle, GWL_EXSTYLE, AStoreWindowLong);
  end;
end;

procedure TfmAlertWindowCustomDraw.awmCustomDrawDemo1CustomDrawMessageText(
  Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas;
  AViewInfo: TdxAlertWindowMessageTextViewInfo; var ADone: Boolean);
var
  AMinCharIndex, AMaxCharIndex: Integer;
begin
  GetRichEditVisibleRange(redtMessageText.InnerControl.Handle, AMinCharIndex, AMaxCharIndex);
  DrawTransparentRichEdit(ACanvas, AViewInfo.Bounds, redtMessageText, AMinCharIndex, AMaxCharIndex);
  ADone := True;
end;

procedure TfmAlertWindowCustomDraw.awmCustomDrawDemo1MeasureMessageText(
  Sender: TObject; AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer);
begin
  AWidth := cxRectWidth(redtMessageText.InnerControl.ClientRect) - 1;
  AHeight := cxRectHeight(redtMessageText.InnerControl.ClientRect);
end;

procedure TfmAlertWindowCustomDraw.btnAlignLeftClick(Sender: TObject);
begin
  redtMessageText.Paragraph.Alignment := taLeftJustify;
end;

procedure TfmAlertWindowCustomDraw.btnAlignCenterClick(Sender: TObject);
begin
  redtMessageText.Paragraph.Alignment := taCenter;
end;

procedure TfmAlertWindowCustomDraw.btnAlignRightClick(Sender: TObject);
begin
  redtMessageText.Paragraph.Alignment := taRightJustify;
end;

procedure TfmAlertWindowCustomDraw.btnFontClick(Sender: TObject);
begin
  fdMessageText.Font.Color := redtMessageText.SelAttributes.Color;
  fdMessageText.Font.Name := redtMessageText.SelAttributes.Name;
  fdMessageText.Font.Size := redtMessageText.SelAttributes.Size;
  fdMessageText.Font.Style := redtMessageText.SelAttributes.Style;
  fdMessageText.Font.Pitch := redtMessageText.SelAttributes.Pitch;
  if fdMessageText.Execute then
    SetSelAttributes;
end;

procedure TfmAlertWindowCustomDraw.btnShowClick(Sender: TObject);
begin
  awmCustomDrawDemo1.Show('Andrew Fuller', '', 0);
end;

procedure TfmAlertWindowCustomDraw.fdMessageTextApply(Sender: TObject; Wnd: HWND);
begin
  SetSelAttributes;
end;

procedure TfmAlertWindowCustomDraw.GetSelAttributes;
begin
  btnBold.Down := fsBold in redtMessageText.SelAttributes.Style;
  btnItalic.Down := fsItalic in redtMessageText.SelAttributes.Style;
  btnUnderline.Down := fsUnderline in redtMessageText.SelAttributes.Style;
  cbbFontName.Text := redtMessageText.SelAttributes.Name;
  cbbFontName.Properties.FontPreview.FontStyle := redtMessageText.SelAttributes.Style;
  cbTextSize.Text := IntToStr(Abs(redtMessageText.SelAttributes.Size));
  ccbTextColor.ColorValue := redtMessageText.SelAttributes.Color;
  case redtMessageText.Paragraph.Alignment of
    taLeftJustify: btnAlignLeft.Down := True;
    taCenter: btnAlignCenter.Down := True;
    taRightJustify: btnAlignRight.Down := True;
  end;
end;

procedure TfmAlertWindowCustomDraw.SetSelAttributes;
begin
  redtMessageText.SelAttributes.Color := fdMessageText.Font.Color;
  redtMessageText.SelAttributes.Name := fdMessageText.Font.Name;
  redtMessageText.SelAttributes.Size := fdMessageText.Font.Size;
  redtMessageText.SelAttributes.Style := fdMessageText.Font.Style;
  redtMessageText.SelAttributes.Pitch := fdMessageText.Font.Pitch;
end;

procedure TfmAlertWindowCustomDraw.redtMessageTextPropertiesSelectionChange(Sender: TObject);
begin
  GetSelAttributes;
  RedrawAlertWindows;
end;

procedure TfmAlertWindowCustomDraw.btnBoldClick(Sender: TObject);
var
  AFontStyle: TFontStyles;
begin
  AFontStyle := redtMessageText.SelAttributes.Style;
  if btnBold.Down then
    Include(AFontStyle, fsBold)
  else
    Exclude(AFontStyle, fsBold);
  redtMessageText.SelAttributes.Style := AFontStyle;
end;

procedure TfmAlertWindowCustomDraw.btnItalicClick(Sender: TObject);
var
  AFontStyle: TFontStyles;
begin
  AFontStyle := redtMessageText.SelAttributes.Style;
  if btnItalic.Down then
    Include(AFontStyle, fsItalic)
  else
    Exclude(AFontStyle, fsItalic);
  redtMessageText.SelAttributes.Style := AFontStyle;
end;

procedure TfmAlertWindowCustomDraw.btnUnderlineClick(Sender: TObject);
var
  AFontStyle: TFontStyles;
begin
  AFontStyle := redtMessageText.SelAttributes.Style;
  if btnUnderline.Down then
    Include(AFontStyle, fsUnderline)
  else
    Exclude(AFontStyle, fsUnderline);
  redtMessageText.SelAttributes.Style := AFontStyle;
end;

procedure TfmAlertWindowCustomDraw.cbbFontNamePropertiesChange(Sender: TObject);
begin
  if cbbFontName.Focused then
    redtMessageText.SelAttributes.Name := cbbFontName.Text;
end;

procedure TfmAlertWindowCustomDraw.cbbFontNamePropertiesFontPreviewButtonClick(
  Sender: TObject; ButtonType: TcxFontButtonType);
begin
  redtMessageText.SelAttributes.Style := cbbFontName.Properties.FontPreview.FontStyle;
end;

procedure TfmAlertWindowCustomDraw.FormShow(Sender: TObject);
begin
  redtMessageText.Lines.LoadFromFile(DefaultFileName);
  GetSelAttributes;
end;

procedure TfmAlertWindowCustomDraw.Open1Click(Sender: TObject);
var
  AOpenDialog: TOpenDialog;
begin
  AOpenDialog := TOpenDialog.Create(Self);
  AOpenDialog.Filter := RTFFilter;
  if AOpenDialog.Execute then
  begin
    redtMessageText.Clear;
    redtMessageText.Lines.LoadFromFile(AOpenDialog.FileName);
  end;
  FreeAndNil(AOpenDialog);
end;

procedure TfmAlertWindowCustomDraw.Save1Click(Sender: TObject);
var
  ASaveDialog: TSaveDialog;
begin
  ASaveDialog := TSaveDialog.Create(Self);
  ASaveDialog.Filter := RTFFilter;
  if ASaveDialog.Execute then
  begin
    ASaveDialog.FileName := ChangeFileExt(ASaveDialog.FileName, '.rtf');
    redtMessageText.Lines.SaveToFile(ASaveDialog.FileName);
  end;
  FreeAndNil(ASaveDialog);
end;

procedure TfmAlertWindowCustomDraw.cbTextSizePropertiesChange(Sender: TObject);
begin
  if cbTextSize.Focused then
    redtMessageText.SelAttributes.Size := StrToInt(cbTextSize.Text);
end;

procedure TfmAlertWindowCustomDraw.ccbTextColorPropertiesChange(Sender: TObject);
begin
  if ccbTextColor.Focused then
    redtMessageText.SelAttributes.Color := ccbTextColor.ColorValue;
end;

procedure TfmAlertWindowCustomDraw.RedrawAlertWindows;
var
  I: Integer;
begin
  for I := 0 to awmCustomDrawDemo1.Count - 1 do
    awmCustomDrawDemo1[I].Invalidate;
end;

procedure TfmAlertWindowCustomDraw.awmCustomDrawDemo1BeforeShow(
  Sender: TObject; AAlertWindow: TdxAlertWindow);
begin
  AAlertWindow.Top := Top;
  if Left - Screen.WorkAreaRect.Left >= Screen.WorkAreaRect.Right - (Left + Width) then
  begin
    AAlertWindow.Left := Left - AAlertWindow.Width;
    AAlertWindow.OptionsAnimate.ShowingAnimationDirection := awmdLeft;
  end
  else
  begin
    AAlertWindow.Left := Left + Width;
    AAlertWindow.OptionsAnimate.ShowingAnimationDirection := awmdRight;
  end;
end;

procedure TfmAlertWindowCustomDraw.redtMessageTextPropertiesChange(Sender: TObject);
begin
  RedrawAlertWindows;
end;

procedure TfmAlertWindowCustomDraw.FormCreate(Sender: TObject);
begin
  RootLookAndFeel.NativeStyle := True;
end;

end.
