unit StylesSimpleDemoEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxControls, cxContainer, cxEdit, cxMaskEdit,
  cxButtonEdit, cxStyles, cxTextEdit, ExtDlgs, cxLookAndFeelPainters,
  cxButtons, ExtCtrls;

type
  TStylesSimpleDemoEditForm = class(TForm)
    ColorDialog: TColorDialog;
    FontDialog: TFontDialog;
    DesignGroupBox: TGroupBox;
    lbColor: TLabel;
    lbTextColor: TLabel;
    lbFont: TLabel;
    btnedFont: TcxButtonEdit;
    btnedTextColor: TcxButtonEdit;
    btnedColor: TcxButtonEdit;
    lbColorValue: TLabel;
    lbTextColorValue: TLabel;
    btnedBitmap: TcxButtonEdit;
    lbBitmap: TLabel;
    imgExample: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    btnOK1: TcxButton;
    nbtCancel1: TcxButton;
    procedure btnedTextColorKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure nbtCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnedColorPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure btnedFontPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnedBitmapPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
  private
    HoldColor: TColor;
    HoldTextColor: TColor;
    HoldFont: TFont;
    HoldBitmap: TBitmap;
    FSampleBitmap: TBitmap;
    FStyle: TcxStyle;
    procedure SaveStyleParams;
    procedure RestoreStyleParams;
    procedure RefreshStyleInfo;
  public
   property CurrentStyle: TcxStyle read FStyle;
  end;

 function ChangeStyle(AStyle: TcxStyle) : boolean;


implementation

{$R *.dfm}

function ChangeStyle(AStyle: TcxStyle) : boolean;
begin
 with TStylesSimpleDemoEditForm.Create(Application) do
  try
    FStyle := AStyle;
    Caption := 'Edit Style - ' + AStyle.Name;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TStylesSimpleDemoEditForm.btnedTextColorKeyPress(Sender: TObject;
  var Key: Char);
begin
  Key := #0;
end;

procedure TStylesSimpleDemoEditForm.FormShow(Sender: TObject);
begin
  SaveStyleParams;
  RefreshStyleInfo;
end;

procedure TStylesSimpleDemoEditForm.RefreshStyleInfo;
begin
  btnedColor.Style.Color := CurrentStyle.Color;
  lbColorValue.Caption := ColorToString(CurrentStyle.Color);

  btnedTextColor.Style.Color := CurrentStyle.TextColor;
  lbTextColorValue.Caption := ColorToString(CurrentStyle.TextColor);

  btnedFont.Text := CurrentStyle.Font.Name;


  FSampleBitmap.Canvas.Brush.Style := bsSolid;
  if CurrentStyle.Bitmap.Empty then
  begin
    FSampleBitmap.Canvas.Brush.Color := CurrentStyle.Color;
    btnedBitmap.Text := '(none)';
  end
  else
  begin
    FSampleBitmap.Canvas.Brush.Bitmap := CurrentStyle.Bitmap;
    btnedBitmap.Text := '(bitmap)';
  end;
  FSampleBitmap.Canvas.FillRect(Rect(0, 0, FSampleBitmap.Width, FSampleBitmap.Height));
  FSampleBitmap.Canvas.Brush.Style := bsClear;
  FSampleBitmap.Canvas.Font.Assign(CurrentStyle.Font);
  FSampleBitmap.Canvas.Font.Color := CurrentStyle.TextColor;
  FSampleBitmap.Canvas.TextOut(10, 10, 'Style sample here.');
  imgExample.Picture.Bitmap := FSampleBitmap;

end;

procedure TStylesSimpleDemoEditForm.nbtCancelClick(Sender: TObject);
begin
  RestoreStyleParams;
end;

procedure TStylesSimpleDemoEditForm.RestoreStyleParams;
begin
  CurrentStyle.Color := HoldColor;
  CurrentStyle.TextColor := HoldTextColor;
  CurrentStyle.Font := HoldFont;
  CurrentStyle.Font.Assign(HoldFont);
  CurrentStyle.Bitmap := HoldBitmap;
  CurrentStyle.Bitmap.Assign(HoldBitmap);
end;

procedure TStylesSimpleDemoEditForm.SaveStyleParams;
begin
  HoldColor := CurrentStyle.Color;
  HoldTextColor := CurrentStyle.TextColor;
  HoldFont.Assign(CurrentStyle.Font);
  HoldBitmap.Assign(CurrentStyle.Bitmap);
end;

procedure TStylesSimpleDemoEditForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
   if ModalResult <> mrOK then
     RestoreStyleParams;
end;

procedure TStylesSimpleDemoEditForm.btnedColorPropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
   if ColorDialog.Execute then
  begin
     if TComponent(Sender).Tag = 0 then
       CurrentStyle.Color := ColorDialog.Color
     else
       CurrentStyle.TextColor := ColorDialog.Color;
     RefreshStyleInfo;
  end;
end;

procedure TStylesSimpleDemoEditForm.btnedFontPropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  FontDialog.Font.Assign(CurrentStyle.Font);
  if FontDialog.Execute then
  begin
    CurrentStyle.Font := FontDialog.Font;
    RefreshStyleInfo;
  end;
end;

procedure TStylesSimpleDemoEditForm.FormCreate(Sender: TObject);
begin
  HoldFont := TFont.Create;
  HoldBitmap := TBitmap.Create;
  FSampleBitmap := Tbitmap.Create;
  FSampleBitmap.Width := imgExample.Width;
  FSampleBitmap.Height := imgExample.Height;
  imgExample.Picture.Bitmap := FSampleBitmap;
end;

procedure TStylesSimpleDemoEditForm.btnedBitmapPropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  case AButtonIndex of
    0:
    begin
      if OpenPictureDialog.Execute then
        CurrentStyle.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    end;
    1:
    begin
      CurrentStyle.Bitmap.FreeImage;
      CurrentStyle.Bitmap.ReleaseHandle;
      CurrentStyle.AssignedValues := CurrentStyle.AssignedValues - [svBitmap];
    end;
  end;
  RefreshStyleInfo;
end;

procedure TStylesSimpleDemoEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  HoldFont.Free;
  HoldBitmap.Free;
  FSampleBitmap.Free;
end;

end.
