unit RealtorWorldMain;

interface

uses
  Forms, DB, Windows, Messages, SysUtils, Variants, Graphics, RealtorWorldDM,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxCustomTileControl,
  cxClasses, Classes, Controls, dxTileControl, cxImage, dxGDIPlusClasses,
  dxSkinsForm, dxSkinsDefaultPainters, RealtorWorldListing, RealtorWorldAgents,
  RealtorWorldResearch, RealtorWorldUnderConstruction, RealtorWorldLoanCalculator,
  RealtorWorldMortgageRate, RealtorWorldStatistic, ShellApi;

type                                                    
  TfrmRealtorWorld = class(TForm)
    dxSkinController1: TdxSkinController;
    dxTile: TdxTileControl;
    dxTiledxTileControlGroup1: TdxTileControlGroup;
    dxTiledxTileControlGroup2: TdxTileControlGroup;
    dxTiledxTileControlGroup3: TdxTileControlGroup;
    tcaBlackTheme: TdxTileControlActionBarItem;
    tcaClearSelection: TdxTileControlActionBarItem;
    tcaExit: TdxTileControlActionBarItem;
    tcaMakeTileItemLarger: TdxTileControlActionBarItem;
    tcaMakeTileItemSmaller: TdxTileControlActionBarItem;
    tcaWhiteTheme: TdxTileControlActionBarItem;
    tlAgents: TdxTileControlItem;
    tlLoanCalculator: TdxTileControlItem;
    tlMortgageRates: TdxTileControlItem;
    tlPhotos: TdxTileControlItem;
    tlResearch: TdxTileControlItem;
    tlStatistics: TdxTileControlItem;
    tlSystemInformation: TdxTileControlItem;
    tlUserManagement: TdxTileControlItem;
    tlZillow: TdxTileControlItem;
    procedure FormCreate(Sender: TObject);
    procedure tcaChangeThemeClick(Sender: TdxTileControlActionBarItem);
    procedure tcaExitClick(Sender: TdxTileControlActionBarItem);
    procedure tlActivateDetail(Sender: TdxTileControlItem);
    procedure tlUnderConstructionClick(Sender: TdxTileControlItem);
    procedure tlZillowClick(Sender: TdxTileControlItem);
    procedure dxTileItemCheck(Sender: TdxCustomTileControl; AItem: TdxTileControlItem);
    procedure tcaMakeTileItemLargerClick(Sender: TdxTileControlActionBarItem);
    procedure tcaClearSelectionClick(Sender: TdxTileControlActionBarItem);
  private
    procedure SelectSkin(ABlackSkin: Boolean);
    procedure InitializeTileControlItemPhotos;
    procedure InitializeTileControlItemAgents;
    procedure UpdateActionBarsItems;
  end;

var
  frmRealtorWorld: TfrmRealtorWorld;


implementation

uses
  RealtorWorldBaseFrame;

{$R *.dfm}

procedure TfrmRealtorWorld.dxTileItemCheck(
  Sender: TdxCustomTileControl; AItem: TdxTileControlItem);
begin
  UpdateActionBarsItems;
end;

procedure TfrmRealtorWorld.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  dxSkinController1.NativeStyle := False; 
  SelectSkin(True);
  dxTile.LookAndFeel.AssignedValues := [];
  InitializeTileControlItemPhotos;
  InitializeTileControlItemAgents;
  UpdateActionBarsItems;

  dxTile.Controller.StopItemContentAnimation;
  try
    for I := 0 to dxTile.Items.Count - 1 do
      if dxTile.Items[I].Frames.Count > 0 then
        dxTile.Items[I].ActiveFrame := dxTile.Items[I].Frames[0];
  finally
    dxTile.Controller.StartItemContentAnimation;
  end;
  dxSkinController1.TouchMode := True;
end;

procedure TfrmRealtorWorld.InitializeTileControlItemPhotos;
var
  AFrame: TdxTileControlItemFrame;
  AText2, AText3: string;
  AParentID, AHomeID: Integer;
  dsHomes, dsHomesInterior: TDataSet;

  procedure SetTexts(AItem: TdxTileControlCustomItem);
  begin
    AItem.Style.Font.Size := 13;
    AItem.Text2.Value := AText2;
    AItem.Text2.IndentHorz := 0;
    AItem.Text2.IndentVert := 0;
    AItem.Text2.Transparent := False;
    AItem.Text3.Value := AText3;
    AItem.Text3.IndentHorz := 0;
    AItem.Text3.IndentVert := 0;
    AItem.Text3.Transparent := False;
  end;

begin
  dsHomes := DMRealtorWorld.clHomesAndHomes;
  dsHomesInterior := DMRealtorWorld.clHomePhotos;
  dsHomes.First;
  while not dsHomes.EOF do
  begin
    AFrame := tlPhotos.Frames.Add;
    AFrame.Glyph.Mode := ifmFill;
    AFrame.Glyph.Image.LoadFromFieldValue(dsHomes.FieldByName('Photo').Value);
    AFrame.Tag := dsHomes.FieldByName('ID').AsInteger;
    AHomeID := AFrame.Tag;
    AParentID := dsHomes.FieldByName('ID').AsInteger mod 7 + 1;
    AText2 := ' ' + dsHomes.FieldByName('Beds').AsString + ' Beds' + #10 + ' ' + dsHomes.FieldByName('Baths').AsString + ' Baths ';
    AText3 := ' ' + CurrToStrF(dsHomes.FieldByName('Price').AsFloat, ffCurrency, 0) + ' ';
    SetTexts(AFrame);

    dsHomesInterior.Locate('ParentID', AParentID, []);
    while not dsHomesInterior.EOF and
      (dsHomesInterior.FieldByName('ParentID').AsInteger = AParentID) do
    begin
      AFrame := tlPhotos.Frames.Add;
      AFrame.Glyph.Mode := ifmFill;
      AFrame.Glyph.Image.LoadFromFieldValue(dsHomesInterior.FieldByName('Photo').Value);
      AFrame.Tag := AHomeID;
      SetTexts(AFrame);
      dsHomesInterior.Next;
    end;
    dsHomes.Next;
  end;
end;

procedure TfrmRealtorWorld.SelectSkin(ABlackSkin: Boolean);
const
  SkinFileNames: array[Boolean] of string = ('MetroWhite.skinres', 'MetroBlack.skinres');
begin
  dxSkinsUserSkinLoadFromFile(DMRealtorWorld.DataPath + SkinFileNames[ABlackSkin]);
  tcaBlackTheme.Visible := not ABlackSkin;
  tcaWhiteTheme.Visible := ABlackSkin;
end;

procedure TfrmRealtorWorld.InitializeTileControlItemAgents;
var
  AFrame: TdxTileControlItemFrame;
  dsAgents: TDataSet;
begin
  dsAgents := DMRealtorWorld.clHomesAndAgents;
  dsAgents.First;
  while not dsAgents.EOF do
  begin
    AFrame := tlAgents.Frames.Add;
    AFrame.Glyph.Image.LoadFromFieldValue(dsAgents.FieldByName('Photo').Value);
    AFrame.Glyph.Image.Scale(70, 100);
    AFrame.Glyph.Align := oaMiddleRight;
    AFrame.Tag := dsAgents.FieldByName('ID').AsInteger;
    AFrame.Style.Font.Size := 13;
    AFrame.Text1.Value := dsAgents.FieldByName('FirstName').AsString + ' ' + dsAgents.FieldByName('LastName').AsString;
    AFrame.Text1.IndentHorz := 10;
    AFrame.Text1.IndentVert := 10;
    AFrame.Text2.Value := dsAgents.FieldByName('Phone').AsString;
    AFrame.Text2.Align := oaTopLeft;
    AFrame.Text2.IndentHorz := 10;
    AFrame.Text2.IndentVert := 30;
    dsAgents.Next;
  end;
end;

procedure TfrmRealtorWorld.tcaChangeThemeClick(Sender: TdxTileControlActionBarItem);
begin
  SelectSkin(Sender.Tag = 0);
end;

procedure TfrmRealtorWorld.tlActivateDetail(Sender: TdxTileControlItem);
begin
  if Sender.DetailOptions.DetailControl = nil then
    Sender.DetailOptions.DetailControl := GetDetailControlClass(Sender.Tag).Create(Self);
  TfrmBase(Sender.DetailOptions.DetailControl).SelectItem(tlPhotos.ActiveFrame.Tag,
    tlAgents.ActiveFrame.Tag);
end;

procedure TfrmRealtorWorld.tcaExitClick(Sender: TdxTileControlActionBarItem);
begin
  Close;
end;

procedure TfrmRealtorWorld.tcaMakeTileItemLargerClick(Sender: TdxTileControlActionBarItem);
var
  I: Integer;
begin
  for I := 0 to dxTile.CheckedItemCount - 1 do
    dxTile.CheckedItems[I].IsLarge := Sender.Tag = 1;
end;

procedure TfrmRealtorWorld.tlZillowClick(Sender: TdxTileControlItem);
begin
  ShellExecute(0, 'open', 'http://www.zillow.com', nil, nil, SW_SHOW);
end;

procedure TfrmRealtorWorld.UpdateActionBarsItems;
var
  AAllCheckedItemsAreLarge: Boolean;
  AAllCheckedItemsAreSmall: Boolean;
  AItem: TdxTileControlItem;
  I: Integer;
begin
  AAllCheckedItemsAreLarge := True;
  AAllCheckedItemsAreSmall := True;
  for I := 0 to dxTile.CheckedItemCount - 1 do
  begin
    AItem := dxTile.CheckedItems[I];
    AAllCheckedItemsAreLarge := AAllCheckedItemsAreLarge and (AItem.RowCount = 1) and AItem.IsLarge;
    AAllCheckedItemsAreSmall := AAllCheckedItemsAreSmall and (AItem.RowCount = 1) and not AItem.IsLarge;
  end;
  tcaMakeTileItemSmaller.Visible := (dxTile.CheckedItemCount > 0) and AAllCheckedItemsAreLarge;
  tcaMakeTileItemLarger.Visible := (dxTile.CheckedItemCount > 0) and AAllCheckedItemsAreSmall;
  tcaClearSelection.Visible := dxTile.CheckedItemCount > 0;
end;

procedure TfrmRealtorWorld.tlUnderConstructionClick(Sender: TdxTileControlItem);
begin
  if Sender.DetailOptions.DetailControl = nil then
  begin
    Sender.DetailOptions.DetailControl := TfrmUnderConstruction.Create(Self);
    Sender.DetailOptions.DetailControl.Name := Sender.Name + 'DetailControl';
  end;
end;

procedure TfrmRealtorWorld.tcaClearSelectionClick(
  Sender: TdxTileControlActionBarItem);
var
  I: Integer;
begin
  for I := dxTile.CheckedItemCount - 1 downto 0 do
    dxTile.CheckedItems[I].Checked := False;
end;

end.

