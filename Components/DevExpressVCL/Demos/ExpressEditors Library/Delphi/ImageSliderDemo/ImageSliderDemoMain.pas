unit ImageSliderDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, cxControls, SysUtils,
  cxMaskEdit, StdCtrls, ExtCtrls, cxContainer, cxEdit, cxTextEdit, cxDropDownEdit,
  cxDBEdit, cxStyles, Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels,
  cxButtonEdit, dxImageSlider, BaseForm, cxGraphics, cxLookAndFeelPainters,
  cxClasses, DB, dxmdaset, dxGDIPlusClasses, ExtDlgs;

type
  TfrmImageSlider = class(TfmBaseForm)
    miOptions: TMenuItem;
    Add1: TMenuItem;
    Savetofile1: TMenuItem;
    N1: TMenuItem;
    ImageSlider: TdxImageSlider;
    mdsCarOrders: TdxMemData;
    mdsCarOrdersID: TIntegerField;
    mdsCarOrdersTrademark: TStringField;
    mdsCarOrdersModel: TStringField;
    mdsCarOrdersTrademark_Site: TStringField;
    mdsCarOrdersPhoto: TBlobField;
    mdsCarOrdersPrice: TCurrencyField;
    imgCollection: TcxImageCollection;
    Animation1: TMenuItem;
    Preview1: TMenuItem;
    N2: TMenuItem;
    None1: TMenuItem;
    Left1: TMenuItem;
    op1: TMenuItem;
    Right1: TMenuItem;
    Bottom1: TMenuItem;
    ScrollMode1: TMenuItem;
    Buttons1: TMenuItem;
    None2: TMenuItem;
    Slide1: TMenuItem;
    Fadre1: TMenuItem;
    SegmentedFade1: TMenuItem;
    RandomSegementedFade1: TMenuItem;
    issmScrollBar1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    StretchMode1: TMenuItem;
    Normal1: TMenuItem;
    Stretch1: TMenuItem;
    ProportionalStretch1: TMenuItem;
    FitFill1: TMenuItem;
    Fill1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure TrasitionEffectClick(Sender: TObject);
    procedure PreviewPositionClick(Sender: TObject);
    procedure ScollModeClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure Savetofile1Click(Sender: TObject);
    procedure StretchModeClick(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure PopulateImages;
  end;

var
  frmImageSlider: TfrmImageSlider;

implementation

uses
  AboutDemoForm;

{$R *.dfm}

procedure TfrmImageSlider.Add1Click(Sender: TObject);
var
  AGraphic: TdxSmartImage;
begin
  if not OpenPictureDialog1.Execute then
    Exit;

  AGraphic := TdxSmartImage.Create;
  try
    AGraphic.LoadFromFile(OpenPictureDialog1.FileName);
    imgCollection.AddFromMultiFrameImage(AGraphic);
  finally
    AGraphic.Free;
  end;
end;

procedure TfrmImageSlider.FormCreate(Sender: TObject);
begin
  PopulateImages;
end;

procedure TfrmImageSlider.miExitClick(Sender: TObject);
begin
  inherited;
  Exit;
end;

procedure TfrmImageSlider.PopulateImages;
var
  AGraphic: TdxSmartImage;
begin
  AGraphic := TdxSmartImage.Create;
  try
    mdsCarOrders.First;
    imgCollection.Items.BeginUpdate;
    try
      while not mdsCarOrders.Eof do
      begin
        AGraphic.LoadFromFieldValue(mdsCarOrdersPhoto.Value);
        imgCollection.Items.Add().Picture.Graphic := AGraphic;
        mdsCarOrders.Next;
      end;
    finally
      imgCollection.Items.EndUpdate();
    end;
  finally
    AGraphic.Free;
  end;
end;

procedure TfrmImageSlider.SaveToFile1Click(Sender: TObject);
var
  AName: string;
  AImage: TdxSmartImage;
  AExtensions: TStringList;
  AFormat: TdxImageDataFormat;
begin
  if not SavePictureDialog1.Execute then
    Exit;

  AName := SavePictureDialog1.FileName;
  if ExtractFileExt(AName) = '' then
  begin
    AExtensions := TStringList.Create;
    try
      AExtensions.LineBreak := '|';
      AExtensions.Text := SavePictureDialog1.Filter;
      AName := AName + Copy(AExtensions[SavePictureDialog1.FilterIndex * 2 - 1], 2, MaxInt);
    finally
      AExtensions.Free;
    end;
  end;

  if SameText(ExtractFileExt(AName), '.TIFF') or SameText(ExtractFileExt(AName), '.TIF') and (ImageSlider.Images.Count > 1) then
  begin
    AImage := ImageSlider.Images.GetAsMultiFrameTIFF;
    try
      AImage.SaveToFile(AName);
    finally
      AImage.Free;
    end
  end
  else
  begin
    AImage := TdxSmartImage.Create;
    try
      AImage.Assign(ImageSlider.Images.Items[ImageSlider.ItemIndex].Picture.Graphic);
      for AFormat := Low(TdxImageDataFormat) to High(TdxImageDataFormat) do
        if SameText(ExtractFileExt(AName), dxGetImageDataFormatExtension(AFormat)) then
        begin
          AImage.ImageDataFormat := AFormat;
          Break;
        end;
        AImage.SaveToFile(AName);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TfrmImageSlider.ScollModeClick(Sender: TObject);
begin
  ImageSlider.ScrollMode := TdxImageSliderScrollMode(TMenuItem(Sender).Tag);
end;

procedure TfrmImageSlider.StretchModeClick(Sender: TObject);
begin
  ImageSlider.ImageFitMode := TcxImageFitMode(TMenuItem(Sender).Tag);
end;

procedure TfrmImageSlider.TrasitionEffectClick(Sender: TObject);
begin
  ImageSlider.TransitionEffect := TdxImageSliderTransitionEffect(TMenuItem(Sender).Tag);
end;

procedure TfrmImageSlider.PreviewPositionClick(Sender: TObject);
begin
  ImageSlider.PreviewOptions.Position := TcxPosition(TMenuItem(Sender).Tag);
end;


end.

