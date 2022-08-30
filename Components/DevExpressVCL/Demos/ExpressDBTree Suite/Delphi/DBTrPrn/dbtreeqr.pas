unit dbtreeqr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Quickrpt, StdCtrls, ExtCtrls, DB, Qrctrls;

type
  TQRListForm = class(TForm)
    QuickReport: TQuickRep;
    Title: TQRBand;
    PageHeader: TQRBand;
    PageNumber: TQRSysData;
    Detail: TQRBand;
    Image2: TImage;
    QRLabel1: TQRLabel;
    QText: TQRDBText;
    Image: TQRImage;
    ImageRect: TQRImage;
    procedure QuickReportAfterPrint(Sender: TObject);
    procedure QuickReportBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure TitleAfterPrint(Sender: TQRCustomBand; BandPrinted: Boolean);
    procedure TitleBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    ShapeCount : Integer;
    ShapeList : TList;
    ImageLeft, QTextLeft, ImageRectLeft : Integer;
  public
    { Public declarations }
  end;

var
  QRListForm: TQRListForm;

implementation
uses main;

const
  PixelPerLevel = 50;
{$R *.DFM}

procedure HideQRShape(qs : TQRShape);
begin
  with qs do begin
    Visible := False;
    Left := -100;
    Width := 0;
    Height := 0;
  end;
end;

procedure TQRListForm.QuickReportAfterPrint(Sender: TObject);
Var
 i : Integer;
begin
  for i := 0 to ShapeCount do
    TQRShape(ShapeList[i]).Free;
  ShapeList.Free;
end;

procedure TQRListForm.QuickReportBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
Var
 i : Integer;
begin
  ShapeCount := FMain.DBTreePrintDataSet.MaxLevelCount;
  ShapeList := TList.Create;
  ImageLeft := Image.Left;
  QTextLeft := QText.Left;
  ImageRectLeft := ImageRect.Left;
  for i := 0 to ShapeCount do begin
    ShapeList.Add(TQRShape.Create(self));
    with TQRShape(ShapeList[i]) do begin
      Parent := Detail;
      ParentReport := QuickReport;
      if (i = 0) then
        Shape := qrsHorLine
      else Shape := qrsVertLine;
      Pen.Style := psDot;
      HideQRShape(TQRShape(ShapeList[i]));
    end;
  end;

end;

procedure TQRListForm.TitleAfterPrint(Sender: TQRCustomBand;
  BandPrinted: Boolean);
Var
 i : Integer;
begin
  if (Sender <> Detail) then exit;

  Image.Left := ImageLeft;
  QText.Left := QTextLeft;
  ImageRect.Left := ImageRectLeft;
  if Not ImageRect.Visible then begin
    ImageRect.Height := 9;
  end;
  ImageRect.Visible := True;
  for i := 0 to ShapeCount do
    HideQRShape(TQRShape(ShapeList[i]));

end;

procedure TQRListForm.TitleBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
Var
  i, Level, inten : Integer;
  qs : TQRShape;
begin
  if (Sender <> Detail) then exit;
  Level := FMain.DBTreePrintDataSet.FindField('dx$level').AsInteger;
  ImageRect.Visible := FMain.DBTreePrintDataSet.FindField('dx$haschildren').AsBoolean;
  inten := Level * PixelPerLevel;
  Image.Left := Image.Left + inten;
  QText.Left := QText.Left + inten;
  ImageRect.Left := ImageRect.Left + inten;
  qs := TQRShape(ShapeList[0]);
  qs.Visible := True;
  if ImageRect.Visible then
    qs.Left := ImageRect.Left + 1 + ImageRect.Width
  else begin
    qs.Left := ImageRect.Left + ImageRect.Width div 2 - PixelPerLevel;
    ImageRect.Height := 1;
  end;

  qs.Width := Image.Left - qs.Left - 1;
  qs.Top :=  ImageRect.Top + ImageRect.Height div 2;
 
  for i := 0 to level - 1 do begin
    qs := TQRShape(ShapeList[i + 1]);
    qs.Visible := True;
    qs.Left := ImageRectLeft + ImageRect.Width div 2 + i * PixelPerLevel;
    qs.Height := Detail.Height;
  end;

  FMain.ImageList1.GetBitmap(0, Image.Picture.Bitmap);
end;

end.
