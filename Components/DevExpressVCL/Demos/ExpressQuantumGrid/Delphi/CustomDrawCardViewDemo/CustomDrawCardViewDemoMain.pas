unit CustomDrawCardViewDemoMain;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookupGrid, cxLookupDBGrid, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxGridCustomView, cxGrid,
  cxGridCardView, cxGridDBCardView, cxMaskEdit, StdCtrls, ExtCtrls,
  cxContainer, cxEdit, cxTextEdit, cxDropDownEdit, cxDBEdit, cxStyles,
  Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels, cxGraphics,
  ToolWin, cxCustomData, cxFilter, cxData, DB, cxDBData,
  cxClasses, cxDataStorage, cxDBLookupComboBox, cxBlobEdit, DemoUtils,
  BaseForm, cxLookAndFeelPainters, cxGridCustomLayoutView;

type
  TCustomDrawCardViewDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    miSeparator4: TMenuItem;
    ilPics: TImageList;
    miBlue: TMenuItem;
    miGold: TMenuItem;
    miGreen: TMenuItem;
    miGrey: TMenuItem;
    pnPersonLines: TPanel;
    pnPersonLinesCaption: TPanel;
    tvPersonLine: TcxGridDBTableView;
    lvPersonLine: TcxGridLevel;
    cxgPersonLine: TcxGrid;
    pnPersons: TPanel;
    cxgPersons: TcxGrid;
    cvPersons: TcxGridDBCardView;
    lvPersons: TcxGridLevel;
    Splitter: TSplitter;
    pnPersonsCaption: TPanel;
    tvPersonLineNAME: TcxGridDBColumn;
    cvPersonsFIRSTNAME: TcxGridDBCardViewRow;
    cvPersonsSECONDNAME: TcxGridDBCardViewRow;
    cvPersonsGENDER: TcxGridDBCardViewRow;
    cvPersonsBIRTHNAME: TcxGridDBCardViewRow;
    cvPersonsDATEOFBIRTH: TcxGridDBCardViewRow;
    cvPersonsBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvPersonsBIOGRAPHY: TcxGridDBCardViewRow;
    cvPersonsNICKNAME: TcxGridDBCardViewRow;
    miCustomDrawStyles: TMenuItem;
    miGradient: TMenuItem;
    miBackgroundImage: TMenuItem;
    miCar: TMenuItem;
    miSky: TMenuItem;
    miLoadImage: TMenuItem;
    OpenDialog: TOpenDialog;
    miDefaultDrawing: TMenuItem;
    FontDialog: TFontDialog;
    miFont: TMenuItem;
    miDependOnDataDrawing: TMenuItem;
    miEgypt: TMenuItem;
    miTile: TMenuItem;
    miMyFace: TMenuItem;
    cxStyleRepository1: TcxStyleRepository;
    stBlueDark: TcxStyle;
    stGold: TcxStyle;
    stBlueLight: TcxStyle;
    stBlueBright: TcxStyle;
    stYellowLight: TcxStyle;
    stGreyLight: TcxStyle;
    stBlueSky: TcxStyle;
    stDefault: TcxStyle;
    procedure cvPersonsCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure miBlueClick(Sender: TObject);
    procedure miGreenClick(Sender: TObject);
    procedure miGoldClick(Sender: TObject);
    procedure miGreyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miCarClick(Sender: TObject);
    procedure miSkyClick(Sender: TObject);
    procedure miLoadClick(Sender: TObject);
    procedure miDefaultDrawingClick(Sender: TObject);
    procedure miFontClick(Sender: TObject);
    procedure miDependOnDataDrawingClick(Sender: TObject);
    procedure miMyFaceClick(Sender: TObject);
    procedure miEgyptClick(Sender: TObject);
    procedure miTileClick(Sender: TObject);
  private
    FCarBitmap, FSkyBitmap: TBitMap;
    FColorScheme: TColorScheme;
    FCustomDrawingStyle: TCustomDrawingStyle;
    FEgyptBitmap, FMyFaceBitmap, FTileBitmap: TBitMap;
    FFont: TFont;
    FUserDefinedImage, FCurrentBitmap: TBitMap;
    procedure GridsStyles;
    procedure AssignCustomDrawProc;
    procedure SetBkImage(AMenuItem: TObject; ABitMap: TBitMap);
    procedure SetGradientColor(AMenuItem: TObject;
      AColorScheme: TColorScheme; ABackgroudStyle: TcxStyle);
    procedure UncheckMenuItems;
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  CustomDrawCardViewDemoMainForm: TCustomDrawCardViewDemoMainForm;

implementation

{$R *.dfm}

uses
  FilmsDemoData, AboutDemoForm;

procedure TCustomDrawCardViewDemoMainForm.FormCreate(Sender: TObject);
begin
  FColorScheme := csBlue;
  GridsStyles;

  FCustomDrawingStyle := cdsBkImage;

  FFont := TFont.Create;
  FFont.Assign(cxgPersons.Font);

  FCarBitmap := TBitMap.Create;
  LoadImageFromRes(FCarBitmap, 'CAR');

  FSkyBitmap := TBitMap.Create;
  LoadImageFromRes(FSkyBitmap, 'SKY');

  FEgyptBitmap := TBitmap.Create;
  LoadImageFromRes(FEgyptBitmap, 'EGYPT');

  FMyFaceBitmap := TBitmap.Create;
  LoadImageFromRes(FMyFaceBitmap, 'MYFACE');

  FTileBitmap := TBitmap.Create;
  LoadImageFromRes(FTileBitmap, 'TILE');

  FUserDefinedImage := TBitmap.Create;
  FCurrentBitmap := FTileBitmap;

  FilmsDemoDM.cdsFilmsPersons.MasterSource := FilmsDemoDM.dsPersonLines;
  FilmsDemoDM.cdsFilmsPersons.IndexFieldNames := 'PersonLineID';
  FilmsDemoDM.cdsFilmsPersons.MasterFields := 'ID';
end;

procedure TCustomDrawCardViewDemoMainForm.FormDestroy(Sender: TObject);
begin
  FCurrentBitmap := nil;
  FUserDefinedImage.Free;
  FEgyptBitmap.Free;
  FMyFaceBitmap.Free;
  FTileBitmap.Free;
  FCarBitmap.Free;
  FSkyBitmap.Free;
end;

procedure TCustomDrawCardViewDemoMainForm.cvPersonsCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  ABool: Boolean;
  AIsVertical: Boolean;
  procedure SetTextToDraw;
  begin
    if (AViewInfo is TcxGridCardRowDataViewInfo) then
    begin
      if (AViewInfo.Item.Index = cvPersonsGENDER.Index) then
      begin
        if VarAsType(AViewInfo.GridRecord.Values[cvPersonsGENDER.Index], varBoolean) then
          ATextToDraw := 'Male'
        else
          ATextToDraw := 'Female';
      end
      else
        ATextToDraw := AViewInfo.GridRecord.DisplayTexts[AViewInfo.Item.Index];
    end
    else
      ATextToDraw := VarAsType(AViewInfo.Item.Caption, varString);
  end;

begin
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFont);
  SetTextToDraw;
  case FCustomDrawingStyle of
    cdsBkImage:
      ACanvas.FillRect(ARec, FCurrentBitmap);
    cdsGradient:
    begin
      AIsVertical := (FColorScheme = csGrey) or (FColorScheme = csGold);
      if AViewInfo is TcxGridCardRowDataViewInfo then
        DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme), 1], ColorScheme[Integer(FColorScheme), 0], 40, AIsVertical)
      else
        DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme), 0], ColorScheme[Integer(FColorScheme), 1], 40, AIsVertical);
    end;
    cdsDependOnData:
    begin
      ACanvas.Canvas.Brush.Style := bsSolid;
      ACanvas.Canvas.Brush.Color := clBlueLight;
      ACanvas.Canvas.FillRect(ARec);
      if VarAsType(AViewInfo.GridRecord.Values[cvPersonsGENDER.Index], varBoolean) then
        ACanvas.Canvas.Font.Color := clBlue
      else
        ACanvas.Canvas.Font.Color := clFuchsia;
      if (AViewInfo is TcxGridCardRowDataViewInfo) and (AViewInfo.Item.Index = cvPersonsGENDER.Index) then
      begin
        ABool := VarAsType(AViewInfo.GridRecord.Values[cvPersonsGENDER.Index], varBoolean);
        ilPics.Draw(ACanvas.Canvas, ARec.Left + 2, ARec.Top, Integer(ABool));
        ATextToDraw := '';
      end;
    end;
  end;

  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
  ACanvas.DrawText(ATextToDraw, TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).TextRect, 0);

  ADone := FCustomDrawingStyle <> cdsDefaultDrawing;
end;

procedure TCustomDrawCardViewDemoMainForm.GridsStyles;
begin
  cvPersons.OptionsView.SeparatorColor := ColorScheme[2, 2];
  cvPersons.Styles.Background := stBlueSky;
end;

procedure TCustomDrawCardViewDemoMainForm.AssignCustomDrawProc;
begin
  if not Assigned(cvPersons.OnCustomDrawCell) then
    cvPersons.OnCustomDrawCell := cvPersonsCustomDrawCell;
end;

procedure TCustomDrawCardViewDemoMainForm.SetGradientColor(
  AMenuItem: TObject; AColorScheme: TColorScheme; ABackgroudStyle: TcxStyle);
begin
  if (FCustomDrawingStyle <> cdsGradient) or (FColorScheme <> AColorScheme) then
  begin
    FColorScheme := AColorScheme;
    if FCustomDrawingStyle <> cdsGradient then
    begin
      FCustomDrawingStyle := cdsGradient;
      UncheckMenuItems;
    end;
    MenuItemSetChecked(AMenuItem, True);
    cvPersons.OptionsView.SeparatorColor := ColorScheme[Integer(AColorScheme), 2];
    cvPersons.Styles.Background := ABackgroudStyle;
    AssignCustomDrawProc;
    cvPersons.Painter.Invalidate;
  end;
end;

procedure TCustomDrawCardViewDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvPersonLine);
end;

procedure TCustomDrawCardViewDemoMainForm.UncheckMenuItems;
begin
  MenuItemCheckSubItemWithTag('miGradient', -1);
  MenuItemCheckSubItemWithTag('miBackgroundImage', -1);
  MenuItemSetChecked('miDependOnDataDrawing', False);
  MenuItemSetChecked('miDefaultDrawing', False);
end;

procedure TCustomDrawCardViewDemoMainForm.miGreenClick(Sender: TObject);
begin
  SetGradientColor(Sender, csGreen, cxStyle2);
end;

procedure TCustomDrawCardViewDemoMainForm.miGoldClick(Sender: TObject);
begin
  SetGradientColor(Sender, csGold, stYellowLight);
end;

procedure TCustomDrawCardViewDemoMainForm.miGreyClick(Sender: TObject);
begin
  SetGradientColor(Sender, csGrey, stGreyLight);
end;

procedure TCustomDrawCardViewDemoMainForm.miBlueClick(Sender: TObject);
begin
  SetGradientColor(Sender, csBlue, stBlueSky);
end;

procedure TCustomDrawCardViewDemoMainForm.miLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if FCustomDrawingStyle <> cdsBkImage then
    begin
      UncheckMenuItems;
      GridsStyles;
      FCustomDrawingStyle := cdsBkImage;
    end;
    MenuItemSetChecked(Sender, True);
    FUserDefinedImage.LoadFromFile(OpenDialog.FileName);
    FCurrentBitmap := FUserDefinedImage;
    AssignCustomDrawProc;
    cvPersons.Painter.Invalidate;
    SetCurrentDir(ExtractFilePath(Application.ExeName));
 end;
end;

procedure TCustomDrawCardViewDemoMainForm.miDefaultDrawingClick(Sender: TObject);
begin
  UncheckMenuItems;
  MenuItemSetChecked(Sender, True);
  FCustomDrawingStyle := cdsDefaultDrawing;
  cvPersons.OnCustomDrawCell := nil;
  cvPersons.Styles.Background := nil;
  cvPersons.OptionsView.SeparatorColor := clBtnFace;
  cvPersons.Painter.Invalidate;
end;

procedure TCustomDrawCardViewDemoMainForm.miFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(FFont);
  if FontDialog.Execute then
  begin
    FFont.Assign(FontDialog.Font);
    cvPersons.Styles.ContentEven.Font.Assign(FontDialog.Font);
    cvPersons.Styles.ContentOdd.Font.Assign(FontDialog.Font);
    cvPersons.LayoutChanged(False);
    cvPersons.Painter.Invalidate;
  end;
end;

procedure TCustomDrawCardViewDemoMainForm.miDependOnDataDrawingClick(Sender: TObject);
begin
  UncheckMenuItems;
  MenuItemSetChecked(Sender, True);
  FCustomDrawingStyle := cdsDependOnData;
  GridsStyles;
  AssignCustomDrawProc;
  cvPersons.Painter.Invalidate;
end;

procedure TCustomDrawCardViewDemoMainForm.miEgyptClick(Sender: TObject);
begin
  SetBkImage(Sender, FEgyptBitmap);
end;

procedure TCustomDrawCardViewDemoMainForm.miMyFaceClick(Sender: TObject);
begin
  SetBkImage(Sender, FMyFaceBitMap);
end;

procedure TCustomDrawCardViewDemoMainForm.miTileClick(Sender: TObject);
begin
  SetBkImage(Sender, FTileBitMap);
end;

procedure TCustomDrawCardViewDemoMainForm.miSkyClick(Sender: TObject);
begin
  SetBkImage(Sender, FSkyBitmap);
end;

procedure TCustomDrawCardViewDemoMainForm.miCarClick(Sender: TObject);
begin
  SetBkImage(Sender, FCarBitmap);
end;

procedure TCustomDrawCardViewDemoMainForm.SetBkImage(AMenuItem: TObject; ABitMap: TBitmap);
begin
  if FCustomDrawingStyle <> cdsBkImage then
  begin
    UncheckMenuItems;
    GridsStyles;
    FCustomDrawingStyle := cdsBkImage;
  end;
  MenuItemSetChecked(AMenuItem, True);
  FCurrentBitmap := ABitMap;
  AssignCustomDrawProc;
  cvPersons.Painter.Invalidate;
end;

end.  
