unit ImageViewerDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxPCdxBarPopupMenu, cxContainer, cxEdit, cxImage, cxDBEdit, cxGroupBox,
  cxPC, dxGallery, dxGalleryControl, DB, DBClient, dxGDIPlusClasses,
  cxCheckBox, cxSplitter, cxTrackBar, cxLabel, cxTextEdit, cxMemo,
  cxRadioGroup, cxClasses, Menus, StdCtrls, cxButtons, dxColorEdit,
  cxColorComboBox, ExtCtrls, cxMaskEdit, cxDropDownEdit, MidasLib,
  dxLayoutLookAndFeels, dxZoomTrackBar, cxSpinEdit, dxToggleSwitch;

type
  TImageViewerDemoMainForm = class(TForm)
    cdsFilms: TClientDataSet;
    cdsFilmsID: TAutoIncField;
    cdsFilmsCAPTION: TStringField;
    cdsFilmsYEAR: TIntegerField;
    cdsFilmsTAGLINE: TStringField;
    cdsFilmsPLOTOUTLINE: TStringField;
    cdsFilmsRUNTIME: TIntegerField;
    cdsFilmsCOLOR: TStringField;
    cdsFilmsPHOTO: TBlobField;
    cdsFilmsICON: TBlobField;
    cdsFilmsWEBSITE: TStringField;
    dxGalleryControl: TdxGalleryControl;
    cxSplitter1: TcxSplitter;
    pnlToolBar: TPanel;
    imToolBar: TImage;
    pmTextSettings: TPopupMenu;
    miInvisible: TMenuItem;
    miBottomSide: TMenuItem;
    miTopSide: TMenuItem;
    miLeftSide: TMenuItem;
    miRightSide: TMenuItem;
    pmColumnSettings: TPopupMenu;
    miAutoColumnCount: TMenuItem;
    miLine: TMenuItem;
    miThreeColumns: TMenuItem;
    miFourColumns: TMenuItem;
    cxLookAndFeelController1: TcxLookAndFeelController;
    sbTextSettings: TcxButton;
    sbColumnSettings: TcxButton;
    tbItemSize: TcxTrackBar;
    lbItemSize: TcxLabel;
    cbSorted: TcxCheckBox;
    sbInfo: TcxButton;
    gbRightPanel: TcxGroupBox;
    miFiveColumns: TMenuItem;
    cdsGenres: TClientDataSet;
    cdsFilmsGenres: TClientDataSet;
    cdsGenresID: TAutoIncField;
    cdsGenresNAME: TStringField;
    cdsFilmsGenresID: TAutoIncField;
    cdsFilmsGenresFILMID: TIntegerField;
    cdsFilmsGenresGENREID: TIntegerField;
    cdsFilmsGenresPHOTO: TBlobField;
    cdsFilmsGenresICON: TBlobField;
    sbResize: TcxButton;
    imgMain: TcxImage;
    tsHidePanel: TdxToggleSwitch;
    procedure FormCreate(Sender: TObject);
    procedure cbSortedClick(Sender: TObject);
    procedure dxGalleryControlItemClick(Sender: TObject;
      AItem: TdxGalleryControlItem);
    procedure miInvisibleClick(Sender: TObject);
    procedure miBottomSideClick(Sender: TObject);
    procedure miTopSideClick(Sender: TObject);
    procedure miLeftSideClick(Sender: TObject);
    procedure miRightSideClick(Sender: TObject);
    procedure sbTextSettingsClick(Sender: TObject);
    procedure miAutoColumnCountClick(Sender: TObject);
    procedure miThreeColumnsClick(Sender: TObject);
    procedure miFourColumnsClick(Sender: TObject);
    procedure sbColumnSettingsClick(Sender: TObject);
    procedure tbItemSizePropertiesChange(Sender: TObject);
    procedure sbInfoClick(Sender: TObject);
    procedure miFiveColumnsClick(Sender: TObject);
    procedure sbResizeClick(Sender: TObject);
    procedure tsHidePanelPropertiesChange(Sender: TObject);
  private
    FCurrentItem: TdxGalleryControlItem;
    FGlyph: TcxBitmap;
    FStoredPosition: TcxPosition;
    procedure Draw;
    procedure PopulateGallery(ASorted: Boolean);
    procedure ResizeGlyph(AScale: Single; AWidth, AHeight: Integer; IsInPixels: Boolean);
    procedure SetDefaultItem;
    procedure SetTextPosition(APosition: TcxPosition);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  ImageViewerDemoMainForm: TImageViewerDemoMainForm;

implementation

{$R *.dfm}

uses
  cxGeometry, AboutDemoForm, ImageViewerDemoResizeImage;

type
  TdxGalleryControlAccess = class(TdxGalleryControl);

{ TForm1 }

procedure TImageViewerDemoMainForm.FormCreate(Sender: TObject);
begin
  cdsFilms.LoadFromFile('..\..\Data\Films.xml');
  cdsFilms.Open;
  cdsGenres.LoadFromFile('..\..\Data\Genres.xml');
  cdsGenres.Open;
  cdsFilmsGenres.LoadFromFile('..\..\Data\FilmsGenres.xml');
  cdsFilmsGenres.Open;

  miBottomSide.Click;
  miAutoColumnCount.Click;
  tbItemSize.Position := 3;

  PopulateGallery(cbSorted.Checked);
  SetDefaultItem;
end;

constructor TImageViewerDemoMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TcxBitmap.Create;
end;

destructor TImageViewerDemoMainForm.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TImageViewerDemoMainForm.PopulateGallery(ASorted: Boolean);
const
  ActionGenre = 'Action';
  ComedyGenre = 'Comedy';
  DramaGenre = 'Drama';
  OtherGenres = 'Other Genres';

  procedure GetGenresIDs(var AActionID, AComedyID, ADramaID: Integer);
  begin
    cdsGenres.First;
    while not cdsGenres.Eof do
    begin
      if ActionGenre = cdsGenres.FieldByName('Name').AsString then
        AActionID := cdsGenres.FieldByName('ID').AsInteger;
      if ComedyGenre = cdsGenres.FieldByName('Name').AsString then
        AComedyID := cdsGenres.FieldByName('ID').AsInteger;
      if DramaGenre = cdsGenres.FieldByName('Name').AsString then
        ADramaID := cdsGenres.FieldByName('ID').AsInteger;
      cdsGenres.Next;
    end;
  end;

  function FilmGenreGroup(AFilmID, AActionID, AComedyID, ADramaID: Integer): Integer;
  var
    AGenreID: Integer;
  begin
    Result := 3;
    cdsFilmsGenres.FindFirst;
    repeat
      if AFilmID = cdsFilmsGenres.FieldByName('FilmID').AsInteger then
      begin
        AGenreID := cdsFilmsGenres.FieldByName('GenreID').AsInteger;
        if AGenreID = AActionID then
          Result := 0;
        if AGenreID = AComedyID then
          Result := 1;
        if AGenreID = ADramaID then
          Result := 2;
      end;
    until not cdsFilmsGenres.FindNext or (Result < 3);
  end;

var
  AStream: TMemoryStream;
  AImage: TdxSmartImage;
  AGroupIndex, AFilmID, AActionID, AComedyID, ADramaID: Integer;
begin
  AGroupIndex := 0;
  TdxGalleryControlAccess(dxGalleryControl).BeginUpdate;
  try
    dxGalleryControl.Gallery.Groups.Clear;
    dxGalleryControl.Gallery.Groups.Add;
    if ASorted then
    begin
      dxGalleryControl.Gallery.Groups.Add;
      dxGalleryControl.Gallery.Groups.Add;
      dxGalleryControl.Gallery.Groups.Add;
      dxGalleryControl.Gallery.Groups[0].Caption := ActionGenre;
      dxGalleryControl.Gallery.Groups[1].Caption := ComedyGenre;
      dxGalleryControl.Gallery.Groups[2].Caption := DramaGenre;
      dxGalleryControl.Gallery.Groups[3].Caption := OtherGenres;
    end;

    GetGenresIDs(AActionID, AComedyID, ADramaID);

    cdsFilms.First;

    AStream := TMemoryStream.Create;
    try
      while not cdsFilms.Eof do
      begin
        if not cdsFilms.Fields.FieldByName('Photo').IsNull then
        begin
          AImage := TdxSmartImage.Create;
          try
            AStream.Position := 0;
            TBLOBField(cdsFilms.Fields.FieldByName('Photo')).SaveToStream(AStream);
            AStream.Position := 0;
            AImage.LoadFromStream(AStream);
            AFilmID := cdsFilms.Fields.FieldByName('ID').AsInteger;
            if ASorted then
              AGroupIndex := FilmGenreGroup(AFilmID, AActionID, AComedyID, ADramaID);
            with dxGalleryControl.Gallery.Groups[AGroupIndex].Items.Add do
            begin
              Glyph.Assign(AImage);
              Caption := cdsFilms.Fields.FieldByName('Caption').AsString;
              Tag := AFilmID;
            end;
          finally
            AImage.Free;
          end;
         end;
        cdsFilms.Next;
      end;
    finally
      AStream.Free;
    end;
  finally
    TdxGalleryControlAccess(dxGalleryControl).EndUpdate;
  end;
end;

procedure TImageViewerDemoMainForm.ResizeGlyph(AScale: Single; AWidth, AHeight: Integer; IsInPixels: Boolean);
var
  ARect: TRect;
  ABitmap: TcxBitmap;
begin
  ARect := cxNullRect;
  if IsInPixels then
  begin
    ARect.Right := AWidth;
    ARect.Bottom := AHeight;
  end
  else
  begin
    ARect.Right := Round(FGlyph.Width * AScale / 100);
    ARect.Bottom := Round(FGlyph.Height * AScale / 100);
  end;

  ABitmap := TcxBitmap.Create;
  try
    ABitmap.Assign(FGlyph);
    FGlyph.SetSize(ARect);
    FGlyph.Canvas.StretchDraw(ARect, ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TImageViewerDemoMainForm.SetDefaultItem;
begin
  if (dxGalleryControl.Gallery.Groups.Count > 0) and (dxGalleryControl.Gallery.Groups[0].ItemCount > 0) then
    dxGalleryControl.Gallery.ClickItem(dxGalleryControl.Gallery.Groups[0].Items[0]);
end;

procedure TImageViewerDemoMainForm.Draw;
begin
  imgMain.Picture.Assign(FGlyph);
end;

procedure TImageViewerDemoMainForm.SetTextPosition(APosition: TcxPosition);
begin
  FStoredPosition := APosition;
  dxGalleryControl.ItemTextPosition := APosition;
end;

procedure TImageViewerDemoMainForm.cbSortedClick(Sender: TObject);
begin
  PopulateGallery(cbSorted.Checked);
  SetDefaultItem;
end;

procedure TImageViewerDemoMainForm.dxGalleryControlItemClick(Sender: TObject;
  AItem: TdxGalleryControlItem);
begin
  FCurrentItem := AItem;
  FGlyph.Assign(AItem.Glyph);
  Draw;
end;

procedure TImageViewerDemoMainForm.tsHidePanelPropertiesChange(Sender: TObject);
begin
  gbRightPanel.Visible := tsHidePanel.Checked;
  cxSplitter1.Enabled := tsHidePanel.Checked;
  if tsHidePanel.Checked then
    cxSplitter1.Left := gbRightPanel.Left - 10;
end;

procedure TImageViewerDemoMainForm.miInvisibleClick(Sender: TObject);
begin
  SetTextPosition(posNone);
end;

procedure TImageViewerDemoMainForm.miBottomSideClick(Sender: TObject);
begin
  SetTextPosition(posBottom);
end;

procedure TImageViewerDemoMainForm.miTopSideClick(Sender: TObject);
begin
  SetTextPosition(posTop);
end;

procedure TImageViewerDemoMainForm.miLeftSideClick(Sender: TObject);
begin
  SetTextPosition(posLeft);
end;

procedure TImageViewerDemoMainForm.miRightSideClick(Sender: TObject);
begin
  SetTextPosition(posRight);
end;

procedure TImageViewerDemoMainForm.sbTextSettingsClick(Sender: TObject);
var
  P: TPoint;
begin
  P := sbTextSettings.ClientToScreen(Point(0, sbTextSettings.Height));
  pmTextSettings.Popup(P.X, P.Y);
end;

procedure TImageViewerDemoMainForm.miAutoColumnCountClick(Sender: TObject);
begin
  dxGalleryControl.ColumnCount := 0;
end;

procedure TImageViewerDemoMainForm.miThreeColumnsClick(
  Sender: TObject);
begin
  dxGalleryControl.ColumnCount := 3;
end;

procedure TImageViewerDemoMainForm.miFourColumnsClick(
  Sender: TObject);
begin
  dxGalleryControl.ColumnCount := 4;
end;

procedure TImageViewerDemoMainForm.miFiveColumnsClick(Sender: TObject);
begin
  dxGalleryControl.ColumnCount := 5;
end;

procedure TImageViewerDemoMainForm.sbColumnSettingsClick(Sender: TObject);
var
  P: TPoint;
begin
  P := sbColumnSettings.ClientToScreen(Point(0, sbTextSettings.Height));
  pmColumnSettings.Popup(P.X, P.Y);
end;

procedure TImageViewerDemoMainForm.tbItemSizePropertiesChange(
  Sender: TObject);

  procedure SetupTextMenu(APosition: TcxPosition);
  begin
    case APosition of
      posTop: miTopSide.Checked := True;
      posBottom: miBottomSide.Checked := True;
      posLeft: miLeftSide.Checked := True;
      posRight: miRightSide.Checked := True;
    else
      miInvisible.Checked := True;
    end;
  end;

var
  AFactor: Integer;
begin
  if tbItemSize.Position = tbItemSize.Properties.Min then
  begin
    if FStoredPosition <> posNone then
    begin
      dxGalleryControl.ItemTextPosition := posRight;
      SetupTextMenu(posRight);
    end;
    dxGalleryControl.ItemImageSize.Size := cxSize(32, 32);
  end
  else
  begin
    dxGalleryControl.ItemTextPosition := FStoredPosition;
    SetupTextMenu(FStoredPosition);
    if tbItemSize.Position = tbItemSize.Properties.Max then
      dxGalleryControl.ItemImageSize.Size := cxSize(0, 0)
    else
    begin
      AFactor := tbItemSize.Position;
      dxGalleryControl.ItemImageSize.Size := cxSize(40 * AFactor, 40 * AFactor);
    end;
  end;
end;

procedure TImageViewerDemoMainForm.sbInfoClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TImageViewerDemoMainForm.sbResizeClick(Sender: TObject);
begin
  ImageViewerDemoResizeImageForm.GlyphHeight := FGlyph.Height;
  ImageViewerDemoResizeImageForm.GlyphWidth := FGlyph.Width;
  if ImageViewerDemoResizeImageForm.ShowModal = mrOK then
  begin
    ResizeGlyph(ImageViewerDemoResizeImageForm.seScale.Value, StrToInt(ImageViewerDemoResizeImageForm.teWidth.Text),
      StrToInt(ImageViewerDemoResizeImageForm.teHeight.Text), ImageViewerDemoResizeImageForm.cgbPixels.CheckBox.Checked);
    FCurrentItem.Glyph.Assign(FGlyph);
  end;
end;

end.
