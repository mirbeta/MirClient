unit InPlaceEditorsDemoComboBoxes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, cxGraphics, cxGeometry, Math, InPlaceEditorsDemoFrameManager,
  StdCtrls;

type
  TfrmComboBoxes = class(TEditorDemoBaseFrame)
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FPath: string;
    FFontName: string;
    FFontSize: Integer;
    FColor: TColor;
    FImage: TBitmap;
    FPerson: string;
  public
    procedure SetParameters(AColor: TColor; const AFontName, AFolder, APerson: string; AFontSize: Integer; AImage: TBitmap);
  end;

implementation

{$R *.dfm}

uses
  Types;

{ TfrmComboBoxes }

procedure TfrmComboBoxes.SetParameters(AColor: TColor; const AFontName, AFolder, APerson: string; AFontSize: Integer; AImage: TBitmap);
begin
  FColor := AColor;
  FFontName := AFontName;
  FFontSize := AFontSize;
  FPath := AFolder;
  FImage.Assign(AImage);
  FPerson := Format('Contact person: %s', [APerson]);
  PaintBox1.Invalidate;
end;

procedure TfrmComboBoxes.FormCreate(Sender: TObject);
begin
  FImage := TBitmap.Create;
end;

procedure TfrmComboBoxes.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TfrmComboBoxes.PaintBox1Paint(Sender: TObject);
var
  ARect: TRect;
  AIndent: Integer;
begin
  if FImage <> nil then
  begin
    PaintBox1.Canvas.Brush.Color := FColor;
    PaintBox1.Canvas.FillRect(PaintBox1.ClientRect);
    PaintBox1.Canvas.Font.Name := FFontName;
    PaintBox1.Canvas.Font.Size := FFontSize;
    PaintBox1.Canvas.Brush.Style := bsClear;
    AIndent := Min((PaintBox1.Width - FImage.Width - PaintBox1.Canvas.TextWidth(FPerson) - 4) div 2,
      (PaintBox1.Width - FImage.Width - PaintBox1.Canvas.TextWidth(FPath) - 4) div 2);
    AIndent := Max(0, AIndent);
    ARect := cxRectBounds(AIndent, (PaintBox1.Height - FImage.Height) div 2, FImage.Width, FImage.Height);
    cxDrawImage(PaintBox1.Canvas.Handle, ARect, ARect, FImage, nil, -1, idmNormal);
    PaintBox1.Canvas.TextOut(ARect.Right + 4, (PaintBox1.Height - PaintBox1.Canvas.TextHeight(FPath)) div 2, FPath);
    PaintBox1.Canvas.TextOut(ARect.Right + 4, (PaintBox1.Height - PaintBox1.Canvas.TextHeight(FPerson)) div 2 + PaintBox1.Canvas.TextHeight(FPath), FPerson);
  end;
end;

end.
