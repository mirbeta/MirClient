unit RealtorWorldHomePhotosBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, DB, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxClasses, dxCustomTileControl, dxTileControl, RealtorWorldBaseFrame,
  cxSplitter;

type
  TfrmHomePhotosBase = class(TfrmBase)
    tcHomePhotos: TdxTileControl;
    tcHomePhotosdxTileControlGroup1: TdxTileControlGroup;
    cxSplitter1: TcxSplitter;
    procedure cxSplitter1BeforeClose(Sender: TObject; var AllowClose: Boolean);
  private
    { Private declarations }
  protected
    procedure OnItemClick(Sender: TdxTileControlItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeFrame; virtual;
    procedure SelectItem(APhotoID, AAgentID: Integer); override;
  end;

implementation

uses
  RealtorWorldDM;

{$R *.dfm}

{ TfrmHomePhotosBase }

constructor TfrmHomePhotosBase.Create(AOwner: TComponent);
begin
  inherited;
  InitializeFrame;
end;

procedure TfrmHomePhotosBase.cxSplitter1BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

procedure TfrmHomePhotosBase.InitializeFrame;
var
  dsHomes: TDataSet;
  AItem: TdxTileControlItem;
begin
  tcHomePhotos.BeginUpdate;
  try
    dsHomes := DMRealtorWorld.clHomesAndHomes;
    dsHomes.First;
    while not dsHomes.EOF do
    begin
      AItem := tcHomePhotos.CreateItem(True);
      AItem.Glyph.Image.LoadFromFieldValue(dsHomes.FieldByName('Photo').Value);
      AItem.Glyph.Mode := ifmStretch;
      AItem.Text2.Value := ' ' + dsHomes.FieldByName('Beds').AsString + ' Beds' + #10 + ' ' + dsHomes.FieldByName('Baths').AsString + ' Baths ';
      AItem.Text2.IndentHorz := 0;
      AItem.Text2.Font.Size := 13;
      AItem.Text2.IndentVert := 0;
      AItem.Text2.Transparent := False;
      AItem.Text3.Value := ' ' + CurrToStrF(dsHomes.FieldByName('Price').AsFloat, ffCurrency, 0) + ' ';
      AItem.Text3.IndentHorz := 0;
      AItem.Text3.IndentVert := 0;
      AItem.Text3.Font.Size := 13;
      AItem.Text3.Transparent := False;
      AItem.Tag := dsHomes.FieldByName('ID').AsInteger;
      AItem.OnClick := OnItemClick;
      dsHomes.Next;
    end;
  finally
    tcHomePhotos.EndUpdate;
  end;
end;

procedure TfrmHomePhotosBase.OnItemClick(Sender: TdxTileControlItem);
begin
end;

procedure TfrmHomePhotosBase.SelectItem(APhotoID, AAgentID: Integer);
var
  I: Integer;
begin
  for I := 0 to tcHomePhotos.Items.Count - 1 do
    if tcHomePhotos.Items[I].Tag = APhotoID then
    begin
      tcHomePhotos.Items[I].MakeVisible;
      tcHomePhotos.Items[I].Click;
      Break;
    end;
end;

end.
