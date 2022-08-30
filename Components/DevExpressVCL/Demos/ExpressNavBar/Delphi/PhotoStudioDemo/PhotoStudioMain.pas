unit PhotoStudioMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, dxCore,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, StdCtrls,
  ExtCtrls, dxNavBar, cxGroupBox, dxNavBarBase, dxNavBarCollns, cxTrackBar,
{$IFDEF EXPRESSSKINS}
  {$I dxSkins.inc}
  dxSkinsForm, dxSkinsdxNavBarAccordionViewPainter,
{$ENDIF}
  cxLabel, dxGalleryControl, dxGallery, dxGDIPlusClasses, cxImage, dxGDIPlusAPI,
  dxRatingControl;

type
  TImageFilterType = (iftPolaroid, iftGrayScale, iftNegative, iftSepia, iftBGR, iftGBR);

  TfrmMain = class(TForm)
    dxNavBar1: TdxNavBar;
    dxNavBar1Group1: TdxNavBarGroup;
    dxNavBar1Group2: TdxNavBarGroup;
    ngFilters: TdxNavBarGroup;
    ngColors: TdxNavBarGroup;
    ngBrightnessContrast: TdxNavBarGroup;
    ngBrightnessContrastControl: TdxNavBarGroupControl;
    tbR: TcxTrackBar;
    tbG: TcxTrackBar;
    tbB: TcxTrackBar;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    tbBrightness: TcxTrackBar;
    tbContrast: TcxTrackBar;
    cxLabel4: TcxLabel;
    cxLabel5: TcxLabel;
    gcFilters: TdxNavBarGroupControl;
    gcColor: TdxNavBarGroupControl;
    dxGalleryControl2: TdxGalleryControl;
    dxGalleryControl1: TdxGalleryControl;
    dxGalleryControl1Group1: TdxGalleryControlGroup;
    cxImage1: TcxImage;
    dxNavBar1Group2Control: TdxNavBarGroupControl;
    cxGroupBox2: TcxGroupBox;
    dxRatingControl1: TdxRatingControl;
    cxImage2: TcxImage;
    lblImageFileName: TcxLabel;
    dxGalleryControl2Group1: TdxGalleryControlGroup;
    lblImageFileInfo: TcxLabel;
    procedure dxGalleryControl2ItemClick(Sender: TObject;
      AItem: TdxGalleryControlItem);
    procedure FormCreate(Sender: TObject);
    procedure dxGalleryControl1ItemClick(Sender: TObject;
      AItem: TdxGalleryControlItem);
    procedure tbContrastPropertiesChange(Sender: TObject);
    procedure tbRGBPropertiesChange(Sender: TObject);
  private
    FChangeLock: Boolean;
    function ApplyBrightness(AImage: TdxSmartImage; ABrightnessValue: Byte): TdxSmartImage;
    function ApplyColorMatrics(AImage: TdxSmartImage; AColorMatrix: TdxGpColorMatrix): TdxSmartImage;
    function ApplyContrast(AImage: TdxSmartImage; AContrastValue: Byte): TdxSmartImage;
    function ApplyFilter(AImage: TdxSmartImage; AFilterType: TImageFilterType): TdxSmartImage;
    function ApplyRGB(AImage: TdxSmartImage; R, G, B: Byte): TdxSmartImage;
    procedure PopulateGalery(APath: string);
    procedure UpdateFilterGroup(AGlyph: TdxSmartGlyph);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  dxNavBarConsts;

const
  SImageFilterName: array [TImageFilterType] of string = ('Polaroid', 'GrayScale', 'Negative', 'Sepia', 'BGR', 'GBR');

{ TdxNavBarControlDemoUnitForm1 }

procedure TfrmMain.dxGalleryControl1ItemClick(
  Sender: TObject; AItem: TdxGalleryControlItem);
begin
  if FChangeLock then
    Exit;
  FChangeLock := True;
  try
    tbBrightness.EditValue := 0;
    tbContrast.EditValue := 0;
    tbR.EditValue := 0;
    tbG.EditValue := 0;
    tbB.EditValue := 0;
    cxImage1.Picture.Graphic := AItem.Glyph;
  finally
    FChangeLock := False;
  end;
end;

procedure TfrmMain.dxGalleryControl2ItemClick(
  Sender: TObject; AItem: TdxGalleryControlItem);
begin
  FChangeLock := True;
  try
    cxImage1.Picture.Graphic := AItem.Glyph;
    cxImage2.Picture.Graphic := AItem.Glyph;
    lblImageFileName.Caption := AItem.Caption;
    lblImageFileInfo.Caption := AItem.Description;
    UpdateFilterGroup(AItem.Glyph);
    tbBrightness.EditValue := 0;
    tbContrast.EditValue := 0;
    tbR.EditValue := 0;
    tbG.EditValue := 0;
    tbB.EditValue := 0;
  finally
    FChangeLock := False;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  inherited;
  dxGalleryControl2.BeginUpdate;
  dxGalleryControl2Group1.Items.Clear;
  PopulateGalery('..\..\Data\*.jpg');
  dxGalleryControl2.ColumnCount := dxGalleryControl2Group1.ItemCount;
  dxGalleryControl2.EndUpdate;
  if dxGalleryControl2Group1.ItemCount = 0 then
  begin
    ShowMessage('No JPG images were found in the Data folder');
    FChangeLock := True;
    Exit;
  end;
  dxGalleryControl2.Gallery.GetFirstItem.Checked := True;
{$IFDEF EXPRESSSKINS}
  dxNavBar1.View := dxNavBarAccordionView;
  RootLookAndFeel.SkinName := 'Office2013White';
  RootLookAndFeel.NativeStyle := False;
  TdxSkinController.Create(Self);
  dxNavBar1.DefaultStyles.ChildGroupCaption.Font.Style := [fsBold];
  dxNavBar1.DefaultStyles.ChildGroupCaptionHotTracked.Font.Style := [fsBold];
  dxNavBar1.DefaultStyles.ChildGroupCaptionPressed.Font.Style := [fsBold];
{$ELSE}
  dxNavBar1.View := dxNavBarVistaExplorerBarView;
{$ENDIF}
end;

procedure TfrmMain.PopulateGalery(APath: string);

  function IsFile(AFindData: TWIN32FindData): Boolean;
  var
    AFileName: string;
  begin
    AFileName := AFindData.cFileName;
    Result := (AFileName <> '.') and (AFileName <> '..') and
      (AFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0);
  end;

  function GetFileName(AFindData: TWIN32FindData): string;
  begin
    Result := AFindData.cFileName;
  end;

var
  AMask: string;
  AFilePath: string;
  AHandle: THandle;
  AFindData: TWIN32FindData;
  ASize: UInt64;
begin
  if DirectoryExists(APath) then
  begin
    AFilePath := IncludeTrailingPathDelimiter(APath);
    APath := AFilePath + '*';
    AMask := '*';
  end
  else
  begin
    AFilePath := ExtractFilePath(APath);
    AMask := Copy(APath, Length(AFilePath) + 1, MaxInt);
  end;
  AHandle := FindFirstFile(PChar(APath), AFindData);
  if AHandle <> INVALID_HANDLE_VALUE then
    try
      repeat
        if IsFile(AFindData) and ((AMask = '*') or SameText(ExtractFileExt(AFindData.cFileName), ExtractFileExt(AMask))) then
        begin
          with dxGalleryControl2Group1.Items.Add do
          begin
            Glyph.LoadFromFile(AFilePath + GetFileName(AFindData));
            Caption := GetFileName(AFindData);
            ASize := (AFindData.nFileSizeHigh shl 32 + AFindData.nFileSizeLow) div 1024;
            Description := IntToStr(Glyph.Width) + 'x' + IntToStr(Glyph.Height) + dxCRLF + IntToStr(ASize) + ' KB';
          end;
        end;
        if not FindNextFile(AHandle, AFindData) then Break;
      until False;
    finally
      Windows.FindClose(AHandle);
    end;
end;

function TfrmMain.ApplyBrightness(AImage: TdxSmartImage;
  ABrightnessValue: Byte): TdxSmartImage;
begin
  Result := ApplyRGB(AImage, ABrightnessValue, ABrightnessValue, ABrightnessValue);
end;

function TfrmMain.ApplyColorMatrics(AImage: TdxSmartImage;
  AColorMatrix: TdxGpColorMatrix): TdxSmartImage;
var
  AAttributes: TdxGPImageAttributes;
  AGpCanvas: TdxGPCanvas;
begin
  Result := TdxSmartImage.CreateSize(AImage.ClientRect);
  AAttributes := TdxGPImageAttributes.Create;
  try
    AAttributes.SetColorMatrix(@AColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
    AGpCanvas := Result.CreateCanvas;
    try
      AGpCanvas.Draw(AImage, AImage.ClientRect, AAttributes);
    finally
      AGpCanvas.Free;
    end;
  finally
    AAttributes.Free;
  end;
end;

function TfrmMain.ApplyContrast(AImage: TdxSmartImage;
  AContrastValue: Byte): TdxSmartImage;

const
  ColorMatrix: TdxGpColorMatrix =
    ((1, 0, 0, 0, 0),
     (0, 1, 0, 0, 0),
     (0, 0, 1, 0, 0),
     (0, 0, 0, 1, 0),
     (0, 0, 0, 0, 1));

var
  AColorMatrix: TdxGpColorMatrix;
  AScale: Single;
  ATranslate: Single;
begin
  AScale := AContrastValue/100;
  ATranslate := (-0.5 * AContrastValue + 0.5) * 255;
  AColorMatrix := ColorMatrix;
  AColorMatrix[0, 0] := ColorMatrix[0, 0] + AScale;
  AColorMatrix[1, 1] := ColorMatrix[1, 1] + AScale;
  AColorMatrix[2, 2] := ColorMatrix[2, 2] + AScale;
  AColorMatrix[0, 4] := ATranslate;
  AColorMatrix[1, 4] := ATranslate;
  AColorMatrix[2, 4] := ATranslate;
  Result := ApplyColorMatrics(AImage, AColorMatrix);
end;

function TfrmMain.ApplyFilter(
  AImage: TdxSmartImage; AFilterType: TImageFilterType): TdxSmartImage;

const
  ColorMatrics: array [TImageFilterType] of TdxGpColorMatrix =
    (
    //  PolaroidFilter: TdxGpColorMatrix =
        ((1.438, -0.062, -0.062, 0, 0),
         (-0.122, 1.378, -0.122, 0, 0),
         (0.016, -0.016, 1.438, 0, 0),
         (0, 0, 0, 1, 0),
         (0.03, 0.05, -0.2, 0, 1)),
  //    GrayScaleFilter: TdxGpColorMatrix =
        ((0.3, 0.3, 0.3, 0, 0),
         (0.59, 0.59, 0.59, 0, 0),
         (0.11, 0.11, 0.11, 0, 0),
         (0, 0, 0, 1, 0),
         (0, 0, 0, 0, 1)),
   //   NegativeFilter: TdxGpColorMatrix =
        ((-1, 0, 0, 0, 0),
         (0, -1, 0, 0, 0),
         (0, 0, -1, 0, 0),
         (0, 0, 0, 1, 0),
         (1, 1, 1, 0, 1)),
    //  SepiaFilter: TdxGpColorMatrix =
        ((0.393, 0.349, 0.272, 0, 0),
         (0.769, 0.686, 0.534, 0, 0),
         (0.189, 0.168, 0.131, 0, 0),
         (0, 0, 0, 1, 0),
         (0, 0, 0, 0, 1)),
    //  BGRFilter: TdxGpColorMatrix =
        ((0, 0, 1, 0, 0),
         (0, 1, 0, 0, 0),
         (1, 0, 0, 0, 0),
         (0, 0, 0, 1, 0),
         (0, 0, 0, 0, 1)),
   //   GBRFilter: TdxGpColorMatrix =
        ((0, 1, 0, 0, 0),
         (0, 0, 1, 0, 0),
         (1, 0, 0, 0, 0),
         (0, 0, 0, 1, 0),
         (0, 0, 0, 0, 1)));

begin
  Result := ApplyColorMatrics(AImage, ColorMatrics[AFilterType]);
end;

function TfrmMain.ApplyRGB(AImage: TdxSmartImage; R, G,
  B: Byte): TdxSmartImage;

const
  ColorMatrix: TdxGpColorMatrix =
    ((1, 0, 0, 0, 0),
     (0, 1, 0, 0, 0),
     (0, 0, 1, 0, 0),
     (0, 0, 0, 1, 0),
     (0.1, 0.1, 0.1, 0, 1));

var
  AColorMatrix: TdxGpColorMatrix;
  I: Integer;
begin
  AColorMatrix := ColorMatrix;
  AColorMatrix[0, 0] := ColorMatrix[0, 0] + R/255;
  AColorMatrix[1, 1] := ColorMatrix[1, 1] + G/255;
  AColorMatrix[2, 2] := ColorMatrix[2, 2] + B/255;
  if (R = 0) and (G = 0) and (B = 0) then
    for I := 0 to 3 do
      AColorMatrix[4, I] := 0;
  Result := ApplyColorMatrics(AImage, AColorMatrix);
end;

procedure TfrmMain.UpdateFilterGroup(AGlyph: TdxSmartGlyph);
var
  AGaleryItem: TdxGalleryControlItem;
  AFilterType: TImageFilterType;
  AImage: TdxSmartImage;
begin
  dxGalleryControl1Group1.Items.Clear;
  for AFilterType := Low(TImageFilterType) to High(TImageFilterType) do
  begin
    AGaleryItem := dxGalleryControl1Group1.Items.Add;
    AGaleryItem.Caption := SImageFilterName[AFilterType];
    AImage := ApplyFilter(AGlyph, AFilterType);
    try
      AGaleryItem.Glyph.Assign(AImage);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TfrmMain.tbContrastPropertiesChange(
  Sender: TObject);
var
  AImage, AImage1: TdxSmartImage;
begin
  if FChangeLock then
    Exit;
  FChangeLock := True;
  try
    tbR.EditValue := 0;
    tbG.EditValue := 0;
    tbB.EditValue := 0;
    dxGalleryControl1.Gallery.UncheckAll;
    tbContrast.Update;
    tbBrightness.Update;
    AImage := ApplyContrast(cxImage2.Picture.Graphic as TdxSmartImage, tbContrast.EditValue);
    AImage1 := ApplyBrightness(AImage, tbBrightness.EditValue);
    cxImage1.Picture.Graphic := AImage1;
    AImage1.Free;
    AImage.Free;
  finally
    FChangeLock := False;
  end;
end;

procedure TfrmMain.tbRGBPropertiesChange(Sender: TObject);
var
  AImage: TdxSmartImage;
begin
  if FChangeLock then
    Exit;
  FChangeLock := True;
  try
    tbBrightness.EditValue := 0;
    tbContrast.EditValue := 0;
    dxGalleryControl1.Gallery.UncheckAll;
    tbR.Update;
    tbG.Update;
    tbB.Update;
    AImage := ApplyRGB(cxImage2.Picture.Graphic as TdxSmartImage, tbR.EditValue, tbG.EditValue, tbB.EditValue);
    cxImage1.Picture.Graphic := AImage;
    AImage.Free;
  finally
    FChangeLock := False;
  end;
end;

end.
