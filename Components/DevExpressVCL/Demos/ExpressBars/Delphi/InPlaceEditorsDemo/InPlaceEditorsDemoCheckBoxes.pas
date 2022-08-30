unit InPlaceEditorsDemoCheckBoxes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, InPlaceEditorsDemoFrameManager, ExtDlgs;

type
  TfrmCheckBoxes = class(TEditorDemoBaseFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    GroupBox2: TGroupBox;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetParameters(ASelectValue, AMultiSelectValue: string; ABlackAndWhite: Boolean);
  end;

implementation

uses
  Types, cxGeometry;

{$R *.dfm}

{ TfrmCheckBoxes }

procedure TfrmCheckBoxes.SetParameters(ASelectValue, AMultiSelectValue: string;
  ABlackAndWhite: Boolean);

  procedure SetColor(AShape: TShape; ASelected: Boolean; AColor: TColor);
  begin
    if ASelected then
    begin
      AShape.Brush.Color := AColor;
      AShape.Pen.Color := AColor;
    end
    else
    begin
      AShape.Brush.Color := clWindow;
      AShape.Pen.Color := $99A8AC;
    end;
  end;

  procedure SetColors(AColors: array of TColor);
  begin
    if AMultiSelectValue = '' then AMultiSelectValue := '000';
    SetColor(Shape1, AMultiSelectValue[1] <> '0', AColors[0]);
    SetColor(Shape2, AMultiSelectValue[2] <> '0', AColors[1]);
    SetColor(Shape3, AMultiSelectValue[3] <> '0', AColors[2]);
    SetColor(Shape4, ASelectValue = '0', AColors[0]);
    SetColor(Shape5, ASelectValue = '1', AColors[1]);
    SetColor(Shape6, ASelectValue = '2', AColors[2]);
  end;

const
  ARGBColors: array[0..2] of TColor = (clRed, clYellow, clLime);
  ABlackAndWhiteColors: array[0..2] of TColor = ($828282, $F8F8F8, $DCDCDC);

begin
  if ABlackAndWhite then
    SetColors(ABlackAndWhiteColors)
  else
    SetColors(ARGBColors);
end;

procedure TfrmCheckBoxes.Panel1Resize(Sender: TObject);
begin
  Panel2.BoundsRect := cxRectCenter(Panel1.ClientRect, Panel2.Width, Panel2.Height);
end;

end.
