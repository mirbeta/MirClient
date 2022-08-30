unit RealtorWorldListing;

interface

uses
  Windows, Messages, DB, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Types,
  dxMessages, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxCustomTileControl, dxTileControl, RealtorWorldDM, cxContainer, cxEdit,
  dxImageSlider, cxSplitter, cxGroupBox, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxDBData, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGridLevel, cxClasses, cxGridCustomView, cxGrid,
  cxImage, cxTextEdit, cxMemo, cxRichEdit, dxGDIPlusClasses, jpeg, cxGeometry,
  cxTrackBar, Menus, StdCtrls, cxButtons, Math, Buttons, RealtorWorldBaseFrame,
  RealtorWorldHomePhotosBase;

const
  UM_MAKEIMAGECENTRE = WM_USER + 1;

type
  TfrmListing = class(TfrmHomePhotosBase)
    cxGroupBox1: TcxGroupBox;
    cxGroupBox2: TcxGroupBox;
    imgsHome: TdxImageSlider;
    reFeatures: TcxRichEdit;
    cxSplitter3: TcxSplitter;
    cxSplitter2: TcxSplitter;
    cxGroupBox3: TcxGroupBox;
    imgHomePlan: TcxImage;
    icSlider: TcxImageCollection;
    icPlans: TcxImageCollection;
    icPlansItem1: TcxImageCollectionItem;
    icPlansItem2: TcxImageCollectionItem;
    icPlansItem3: TcxImageCollectionItem;
    icPlansItem4: TcxImageCollectionItem;
    icPlansItem5: TcxImageCollectionItem;
    procedure cxSplitter3BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
    procedure imgHomePlanMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

     procedure MakeImageCentre(var Message: TWMSize); message UM_MAKEIMAGECENTRE;
  private
    procedure InitializeFeatures;
    procedure InitializeFeaturesOfAgent;
    procedure InitializeFeaturesOfHouse;
    procedure ReplaceInFeatures(const ATokenStr, S: string);
  protected
    procedure OnItemClick(Sender: TdxTileControlItem); override;
  end;


implementation

{$R *.dfm}

uses
  RealtorWorldMain;

type
  TcxImageAccess = class(TcxImage);

procedure TfrmListing.ReplaceInFeatures(const ATokenStr, S: string);
var
  ASelLength: Integer;
begin
  ASelLength := Length(ATokenStr);
  reFeatures.SelStart := reFeatures.FindTexT(ATokenStr, 0, -1, []);
  reFeatures.SelLength := ASelLength;
  reFeatures.SelText := S;
end;

procedure TfrmListing.InitializeFeaturesOfAgent;
var
  AStream: TStringStream;
  APhoto: TdxGPImage;
  AImage: TcxBitmap32;
  ASelLength: Integer;
  AName, APhone, AMail: string;
begin
  ASelLength := Length('__dxImageAgent__');
  reFeatures.SelStart := reFeatures.FindTexT('__dxImageAgent__', 0, -1, []);
  reFeatures.SelLength := ASelLength;
  APhoto := TdxSmartImage.Create;
  AImage := TcxBitmap32.CreateSize(100, 120);
  try
    APhoto.LoadFromFieldValue(DMRealtorWorld.clHomesAndAgents.FieldByName('Photo').Value);
    AImage.cxCanvas.FillRect(AImage.ClientRect, clWindow);
    AImage.Canvas.StretchDraw(cxRectInflate(AImage.ClientRect, -5, -5), APhoto);
    AStream := TStringStream.Create(dxBitmapToRTF(AImage));
    try
      reFeatures.ActiveProperties.StreamModes := [resmSelection];
      reFeatures.Lines.LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  finally
    AImage.Free;
    APhoto.Free;
  end;

  AName := DMRealtorWorld.clHomesAndAgents.FieldByName('FirstName').AsString + ' ' +
    DMRealtorWorld.clHomesAndAgents.FieldByName('LastName').AsString;
  ReplaceInFeatures('__AgentName__', AName);

  APhone := DMRealtorWorld.clHomesAndAgents.FieldByName('Phone').AsString;
  ReplaceInFeatures('__AgentPhone__', APhone);

  AMail := DMRealtorWorld.clHomesAndAgents.FieldByName('EMail').AsString;
  ReplaceInFeatures('__AgentMail__', AMail);
end;

procedure TfrmListing.InitializeFeaturesOfHouse;
var
  AFeatures, AAddress, ARooms, AYearBuilt, ASquares, APrice: string;
begin
  AAddress := DMRealtorWorld.clHomesAndHomes.FieldByName('Address').AsString;
  ReplaceInFeatures('__Address__', AAddress);
  ARooms := DMRealtorWorld.clHomesAndHomes.FieldByName('Beds').AsString + ' bedrooms, ' +
    DMRealtorWorld.clHomesAndHomes.FieldByName('Baths').AsString + ' bathrooms';
  ReplaceInFeatures('__Rooms__', ARooms);

  ASquares := 'House size ' +
    CurrToStrF(DMRealtorWorld.clHomesAndHomes.FieldByName('HouseSize').AsFloat, ffNumber, 0) + ' Sq Ft, ' +
    'lot size ' + Format('%.2f',
    [DMRealtorWorld.clHomesAndHomes.FieldByName('LotSize').AsFloat]) + ' Acres';
  ReplaceInFeatures('__Squares__', ASquares);

  AYearBuilt := 'Built in ' + DMRealtorWorld.clHomesAndHomes.FieldByName('YearBuilt').AsString;
  ReplaceInFeatures('__YearBuilt__', AYearBuilt);

  APrice := CurrToStrF(DMRealtorWorld.clHomesAndHomes.FieldByName('Price').AsCurrency, ffCurrency, 0);
  ReplaceInFeatures('__Price__', APrice);

  AFeatures := Trim(DMRealtorWorld.clHomesAndHomes.FieldByName('Features').AsString);
  AFeatures := StringReplace(AFeatures, ', ', #13, [rfReplaceAll]);

  ReplaceInFeatures('__Features__', AFeatures);
end;

procedure TfrmListing.InitializeFeatures;
begin
  reFeatures.Lines.BeginUpdate;
  try
    reFeatures.Lines.Clear;
    reFeatures.Properties.StreamModes := [];
    reFeatures.Lines.LoadFromFile(DMRealtorWorld.DataPath + 'ListingFmt.rtf');
    reFeatures.Properties.StreamModes := [resmSelection];

    if DMRealtorWorld.clHomesAndAgents.Locate('ID', DMRealtorWorld.clHomesAndHomes.FieldByName('AgentID').AsInteger, []) then
      InitializeFeaturesOfAgent;
    InitializeFeaturesOfHouse;
  finally
    reFeatures.Lines.EndUpdate;
  end;
end;

procedure TfrmListing.OnItemClick(Sender: TdxTileControlItem);
var
  AParentID: Integer;
  dsHomesInterior: TDataSet;
  AGraphic: TdxGPImage;
begin
  imgHomePlan.Picture := icPlans.Items.Items[
    (Sender.Tag - 1) mod icPlans.Items.Count].Picture;
  AParentID := TComponent(Sender).Tag mod 7 + 1;
  icSlider.Items.BeginUpdate;
  try
    icSlider.Items.Clear;
    AGraphic := TdxSmartImage.Create;
    try
      if DMRealtorWorld.clHomesAndHomes.Locate('ID', Sender.Tag, []) then
      begin
        AGraphic.LoadFromFieldValue(DMRealtorWorld.clHomesAndHomes.FieldByName('Photo').Value);
        icSlider.Items.Add.Picture.Graphic := AGraphic;
        InitializeFeatures;
      end;
      PostMessage(Handle, UM_MAKEIMAGECENTRE, 0, 0);
      dsHomesInterior := DMRealtorWorld.clHomePhotos;
      dsHomesInterior.Locate('ParentID', AParentID, []);
      while not dsHomesInterior.EOF and
        (dsHomesInterior.FieldByName('ParentID').AsInteger = AParentID) do
      begin
        AGraphic.LoadFromFieldValue(dsHomesInterior.FieldByName('Photo').Value);
        icSlider.Items.Add.Picture.Graphic := AGraphic;
        dsHomesInterior.Next;
      end;
    finally
      AGraphic.Free;
    end;
    imgsHome.ItemIndex := 0;
  finally
    icSlider.Items.EndUpdate(True);
  end;
end;

procedure TfrmListing.cxSplitter3BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False; 
end;

procedure TfrmListing.imgHomePlanMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TfrmListing.MakeImageCentre(var Message: TWMSize);
begin
  TcxImageAccess(imgHomePlan).Centre;
end;

initialization
  RegisterFrame(IDPhotos, TfrmListing);

end.
