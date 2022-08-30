unit RatingControlDemoImagePicker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, Menus, StdCtrls, cxButtons, cxLabel, dxGDIPlusClasses,
  cxImage, dxRatingControl;

type
  TfrmImagePicker = class(TForm)
    imgUnchecked: TcxImage;
    imgHover: TcxImage;
    imgChecked: TcxImage;
    lbCaption: TcxLabel;
    btnApply: TcxButton;
    btnReset: TcxButton;
    procedure ApplyClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
  private
    FProperties: TdxRatingControlProperties;
  public
    procedure Initialize(AProperties: TdxRatingControlProperties);
  end;

var
  frmImagePicker: TfrmImagePicker;

implementation

{$R *.dfm}

{ TfrmImagePicker }

procedure TfrmImagePicker.ApplyClick(Sender: TObject);
begin
  FProperties.BeginUpdate;
  try
    FProperties.Glyph.Assign(imgUnchecked.Picture.Graphic);
    FProperties.HoverGlyph.Assign(imgHover.Picture.Graphic);
    FProperties.CheckedGlyph.Assign(imgChecked.Picture.Graphic);
  finally
    FProperties.EndUpdate;
  end;
end;

procedure TfrmImagePicker.ResetClick(Sender: TObject);
begin
  FProperties.BeginUpdate;
  try
    FProperties.Glyph.Clear;
    FProperties.HoverGlyph.Clear;
    FProperties.CheckedGlyph.Clear;
  finally
    FProperties.EndUpdate;
  end;
end;

procedure TfrmImagePicker.Initialize(AProperties: TdxRatingControlProperties);
begin
  FProperties := AProperties;
end;

end.
