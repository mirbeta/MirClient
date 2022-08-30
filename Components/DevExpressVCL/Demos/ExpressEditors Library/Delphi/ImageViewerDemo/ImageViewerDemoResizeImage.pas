unit ImageViewerDemoResizeImage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, Menus, StdCtrls, cxButtons, cxTextEdit, cxGroupBox, 
  dxCheckGroupBox, cxMaskEdit, cxSpinEdit, cxLabel, cxCheckBox;

type
  TImageViewerDemoResizeImageForm = class(TForm)
    cxLabel1: TcxLabel;
    seScale: TcxSpinEdit;
    cgbPixels: TdxCheckGroupBox;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    teWidth: TcxTextEdit;
    teHeight: TcxTextEdit;
    btnApply: TcxButton;
    cxButton1: TcxButton;
    cbAspectRatio: TcxCheckBox;
    procedure cgbPixelsPropertiesChange(Sender: TObject);
    procedure teHorizontalPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure teWidthPropertiesChange(Sender: TObject);
    procedure teHeightPropertiesChange(Sender: TObject);
    procedure cbAspectRatioPropertiesChange(Sender: TObject);
  private
    FGlyphHeight: Integer;
    FGlyphWidth: Integer;
    FLockCount: Integer;

    procedure SetGlyphHeight(Value: Integer);
    procedure SetGlyphWidth(Value: Integer);

    procedure CalculateHeight;
    procedure CalculateWidth;
  public
    property GlyphHeight: Integer read FGlyphHeight write SetGlyphHeight;
    property GlyphWidth: Integer read FGlyphWidth write SetGlyphWidth;
  end;

var
  ImageViewerDemoResizeImageForm: TImageViewerDemoResizeImageForm;

implementation

{$R *.dfm}

procedure TImageViewerDemoResizeImageForm.cgbPixelsPropertiesChange(Sender: TObject);
begin
  seScale.Enabled := not cgbPixels.CheckBox.Checked;
end;

procedure TImageViewerDemoResizeImageForm.teHorizontalPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  try
    StrToInt(DisplayValue);
    Error := StrToInt(DisplayValue) < 1;
  except
    Error := True;
  end;
end;

procedure TImageViewerDemoResizeImageForm.teWidthPropertiesChange(
  Sender: TObject);
begin
  CalculateHeight;
end;

procedure TImageViewerDemoResizeImageForm.teHeightPropertiesChange(
  Sender: TObject);
begin
  CalculateWidth;
end;

procedure TImageViewerDemoResizeImageForm.SetGlyphHeight(Value: Integer);
begin
  if FGlyphHeight <> Value then
  begin
    FGlyphHeight := Value;
    teHeight.Text := IntToStr(Value);
  end;
end;

procedure TImageViewerDemoResizeImageForm.SetGlyphWidth(Value: Integer);
begin
  if FGlyphWidth <> Value then
  begin
    FGlyphWidth := Value;
    teWidth.Text := IntToStr(Value);
  end;
end;

procedure TImageViewerDemoResizeImageForm.CalculateHeight;
begin
  if (FLockCount = 0) and (teWidth.Text <> '') and cbAspectRatio.Checked then
  begin
    Inc(FLockCount);
    try
      teHeight.Text := IntToStr(Round(StrToInt(teWidth.Text) * GlyphHeight / GlyphWidth));
    finally
      Dec(FLockCount);
    end;
  end;
end;

procedure TImageViewerDemoResizeImageForm.CalculateWidth;
begin
  if (FLockCount = 0) and (teHeight.Text <> '') and cbAspectRatio.Checked then
  begin
    Inc(FLockCount);
    try
      teWidth.Text := IntToStr(Round(StrToInt(teHeight.Text) * GlyphWidth / GlyphHeight));
    finally
      Dec(FLockCount);
    end;
  end;
end;

procedure TImageViewerDemoResizeImageForm.cbAspectRatioPropertiesChange(
  Sender: TObject);
begin
  CalculateHeight;
end;

end.
