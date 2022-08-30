{$I TMSDEFS.INC}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

unit GDIPPictureContainerEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AdvSmoothImageListBox, SHLObj,
  GDIPPictureContainer, ComCtrls, filectrl, ShellApi, Math,
  AdvGDIP
  ;

type
  TGDIPPictureContainerEditor = class(TForm)
    Panel1: TPanel;
    FileList: TAdvSmoothImageListBox;
    Button2: TButton;
    ImageList: TAdvSmoothImageListBox;
    Button3: TButton;
    CheckBox1: TCheckBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Button1: TButton;
    ComboBox2: TComboBox;
    Button4: TButton;
    Splitter1: TSplitter;
    procedure Button2Click(Sender: TObject);
    procedure ImageListItemZoomIn(Sender: TObject; itemindex: Integer;
      var allow: Boolean);
    procedure ImageListItemStartDrag(Sender: TObject; ItemIndex: Integer;
      var AllowDrag: Boolean);
    procedure FileListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FileListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
    procedure ImageListItemSelect(Sender: TObject; itemindex: Integer);
    procedure ImageListResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ImageListItemClick(Sender: TObject; itemindex: Integer;
      Button: TMouseButton; Shift: TShiftState);
    procedure ImageListItemDraw(Sender: TObject; itemindex: Integer; AItemRect,
      AImageRect: TGPRectF; g: TGPGraphics);
    procedure FileListItemDraw(Sender: TObject; itemindex: Integer; AItemRect,
      AImageRect: TGPRectF; g: TGPGraphics);
    procedure FileListResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileListItemDblClick(Sender: TObject; itemindex: Integer);
    procedure FileListItemStartDrag(Sender: TObject; ItemIndex: Integer;
      var AllowDrag: Boolean);
    procedure FileListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FPictureCol: TPictureCollection;
    { Private declarations }
  public
    { Public declarations }
    procedure Init;
    property PictureCol: TPictureCollection read FPictureCol write FPictureCol;
    procedure PopulateDirectory(AFolder: string);
    procedure AddItems;
    procedure AddItem(Item: TAdvSmoothImageListBoxItem);
  end;


 {$IFNDEF TMSDOTNET}
  TGDIPPictureContainerEditorDialog = class(TCommonDialog)
  {$ENDIF}
  {$IFDEF TMSDOTNET}
  TGDIPPictureContainerEditorDialog = class(TComponent)
  {$ENDIF}
  private
    FForm: TGDIPPictureContainerEditor;
    FCaption: string;
    FPictureCol: TPictureCollection;
    procedure SetPictureCol(const Value: TPictureCollection);
  protected
  public
    {$IFNDEF TMSDOTNET}
    function Execute: Boolean; override;
    {$ENDIF}
    {$IFDEF TMSDOTNET}
    function Execute: Boolean;
    {$ENDIF}
    property Form: TGDIPPictureContainerEditor read FForm;
    property PictureCol: TPictureCollection read FPictureCol write SetPictureCol;
  published
    property Caption: string read FCaption write FCaption;
  end;


var
  Form1: TGDIPPictureContainerEditor;
  CurrentDir: String;

implementation

{$R *.dfm}
{$R GDIPPictureContainer.res}

function FormatByteSize(const bytes: Longint): string;
const
  B = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
begin
  if bytes > GB then
    result := FormatFloat('#.## GB', bytes / GB)
  else
    if bytes > MB then
      result := FormatFloat('#.## MB', bytes / MB)
    else
      if bytes > KB then
        result := FormatFloat('#.## KB', bytes / KB)
      else
        result := FormatFloat('#.## bytes', bytes) ;
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

function GetParentDirectory(path : string) : string;
begin
  result := ExpandFileName(AddBackSlash(path) + '..')
end;

function AddFilter(path,filter: string): string;
begin
  if length(path) > 0 then
  begin
    if path[length(path)] = '\' then
      result := path + filter
    else
      result := path + '\' + filter;
  end
  else
   result := filter;
end;

function PathToMyPictures: string;
var
  Path : pchar;
  idList : PItemIDList;
const
  mypic = 39;
begin
  GetMem(Path, MAX_PATH);
  SHGetSpecialFolderLocation(0, mypic, idList);
  SHGetPathFromIDList(idList, Path);
  Result := string(Path);
  FreeMem(Path);
end;

procedure TGDIPPictureContainerEditor.AddItem(
  Item: TAdvSmoothImageListBoxItem);
begin
  with PictureCol.Add do
  begin
    Picture.LoadFromFile(Item.Location);
    Name := Item.FileName;
  end;

  with FileList.Items.Add do
  begin
    Image.LoadFromFile(Item.Location);
    Caption.Text := Item.FileName;
  end;
end;

procedure TGDIPPictureContainerEditor.AddItems;
var
  I: Integer;
  s: TAdvSmoothImageListBoxItemArray ;
begin
  s := ImageList.Items.SelectedItems;
  for I := 0 to Length(s) - 1 do
    AddItem(s[i]);

  if FileList.Items.Count > 0 then
    FileList.SelectedItemIndex := 0;

  button4.Enabled := FileList.Items.Count > 0;
  button2.Enabled := (FileList.SelectedItemIndex <> -1) and (FileList.Items.Count > 0);
  FileList.SetFocus;
end;

procedure TGDIPPictureContainerEditor.Button1Click(Sender: TObject);
var
  folder: String;
begin
  if SelectDirectory('Select a directory', '', folder) then
    PopulateDirectory(AddBackSlash(folder) + '*.*');
end;

procedure TGDIPPictureContainerEditor.Button2Click(Sender: TObject);
begin
  if (FileList.SelectedItemIndex >= 0) and (FileList.SelectedItemIndex <= FileList.Items.Count - 1) then
  begin
    PictureCol.Delete(FileList.SelectedItemIndex);
    FileList.Items.Delete(FileList.SelectedItemIndex);
  end;

  button4.Enabled := FileList.Items.Count > 0;
  button2.Enabled := (FileList.SelectedItemIndex <> -1) and (FileList.Items.Count > 0);
end;

procedure TGDIPPictureContainerEditor.Button3Click(Sender: TObject);
begin
  AddItems;
end;

procedure TGDIPPictureContainerEditor.Button4Click(Sender: TObject);
begin
  PictureCol.Clear;
  FileList.Items.Clear;
  button4.Enabled := FileList.Items.Count > 0;
  button2.Enabled := (FileList.SelectedItemIndex <> -1) and (FileList.Items.Count > 0);
end;

procedure TGDIPPictureContainerEditor.CheckBox1Click(Sender: TObject);
begin
  TrackBar1.Enabled := not CheckBox1.Checked;
  if CheckBox1.Checked then
    ImageList.Rows := ImageList.Height div ImageList.ItemAppearance.ItemHeight;
end;

procedure TGDIPPictureContainerEditor.ComboBox2Change(Sender: TObject);
begin
  PopulateDirectory(AddBackSlash(CurrentDir) + '*.*');
end;

procedure TGDIPPictureContainerEditor.FileListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  Screen.Cursor := crHourGlass;
  if Source is TAdvSmoothImageListBox then
    AddItems;
  Screen.Cursor := crDefault;
end;

procedure TGDIPPictureContainerEditor.FileListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TAdvSmoothImageListBox;
end;

procedure TGDIPPictureContainerEditor.FileListItemDblClick(Sender: TObject;
  itemindex: Integer);
var
  s: string;
begin
  s := PictureCol.Items[itemindex].Name;
  InputQuery('Picture collection item name',
                'Enter a name for the selected item', s);
  PictureCol.Items[itemindex].Name := s;
  FileList.Items[itemindex].Caption.Text := s;
end;

procedure TGDIPPictureContainerEditor.FileListItemDraw(Sender: TObject;
  itemindex: Integer; AItemRect, AImageRect: TGPRectF; g: TGPGraphics);
var
  ff: TGPFontFamily;
  f: TGPFont;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  sri: TGPRectF;
begin
  if (AImageRect.Height > 0) and ((AItemrect.Height - AImageRect.Height) > 30) then
  begin
    with FileList.Items[Itemindex] do
    begin
      ff := TGPFontFamily.Create(ImageList.TopLayerItems[0].HTMLText.Font.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in ImageList.TopLayerItems[0].HTMLText.Font.Style) then
        fs := fs + 1;
      if (fsItalic in ImageList.TopLayerItems[0].HTMLText.Font.Style) then
        fs := fs + 2;
      if (fsUnderline in ImageList.TopLayerItems[0].HTMLText.Font.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create(StringFormatFlagsNoWrap);
      sf.SetTrimming(StringTrimmingEllipsisWord);
      sf.SetLineAlignment(StringAlignmentCenter);
      f := TGPFont.Create(ff, ImageList.TopLayerItems[0].HTMLText.Font.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(ColorToARGB(ImageList.TopLayerItems[0].HTMLText.Font.Color));

      sri := MakeRect(AItemRect.X + 5, AItemREct.Y + AItemRect.Height - 35, AItemRect.Width - 16, 25);
      ImageList.ItemAppearance.Fill.BeginUpdate;
      ImageList.ItemAppearance.Fill.BorderOpacity := 50;
      ImageList.ItemAppearance.Fill.ShadowOffset := 0;
      ImageList.ItemAppearance.Fill.Fill(g, sri);
      ImageList.ItemAppearance.Fill.ShadowOffset := 5;
      ImageList.ItemAppearance.Fill.BorderOpacity := 255;
      ImageList.ItemAppearance.Fill.EndUpdate;
      sri := MakeRect(sri.X + 2, sri.Y + 2, sri.Width - 4, sri.Height - 4);
      g.DrawString(Caption.Text, Length(Caption.Text), f, sri, sf, b);

      b.Free;
      f.Free;
      sf.Free;
      ff.Free;
    end;
  end;
end;

procedure TGDIPPictureContainerEditor.FileListItemStartDrag(Sender: TObject;
  ItemIndex: Integer; var AllowDrag: Boolean);
begin
  AllowDrag := false;
end;

procedure TGDIPPictureContainerEditor.FileListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
    begin
      FileList.Items.BeginUpdate;
      if (FileList.SelectedItemIndex >= 0) and (FileList.SelectedItemIndex <= FileList.Items.Count - 1) then
      begin
        PictureCol.Delete(FileList.SelectedItemIndex);
        FileList.Items.Delete(FileList.SelectedItemIndex);
      end;

      button4.Enabled := FileList.Items.Count > 0;
      button2.Enabled := (FileList.SelectedItemIndex <> -1) and (FileList.Items.Count > 0);
      FileList.Items.EndUpdate;
    end;
  end;
end;

procedure TGDIPPictureContainerEditor.FileListResize(Sender: TObject);
begin
  FileList.Columns := Max(FileList.Width div FileList.ItemAppearance.ItemWidth, 1);
end;

procedure TGDIPPictureContainerEditor.FormCreate(Sender: TObject);
begin
  Application.HintHidePause := 6000;
end;

procedure TGDIPPictureContainerEditor.ImageListItemClick(Sender: TObject;
  itemindex: Integer; Button: TMouseButton; Shift: TShiftState);
var
  newdir: string;
begin
  if ImageList.Items[ItemIndex].Tag and faDirectory = faDirectory then
  begin
    newdir := ImageList.Items[ItemIndex].FileName;
    PopulateDirectory(AddBackSlash(newdir)+'*.*');
    ImageList.ScrollToItem(0);
    if ImageList.Items.Count > 0 then
      ImageList.SelectedItemIndex := 0;
  end;
end;

procedure TGDIPPictureContainerEditor.ImageListItemDraw(Sender: TObject;
  itemindex: Integer; AItemRect, AImageRect: TGPRectF; g: TGPGraphics);
var
  ff: TGPFontFamily;
  f: TGPFont;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  sri: TGPRectF;
begin
  if (AImageRect.Height > 0) and ((AItemrect.Height - AImageRect.Height) > 30) then
  begin
    with ImageList.Items[Itemindex] do
    begin
      ff := TGPFontFamily.Create(ImageList.TopLayerItems[0].HTMLText.Font.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in ImageList.TopLayerItems[0].HTMLText.Font.Style) then
        fs := fs + 1;
      if (fsItalic in ImageList.TopLayerItems[0].HTMLText.Font.Style) then
        fs := fs + 2;
      if (fsUnderline in ImageList.TopLayerItems[0].HTMLText.Font.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create(StringFormatFlagsNoWrap);
      sf.SetLineAlignment(StringAlignmentCenter);
      sf.SetTrimming(StringTrimmingWord);
      f := TGPFont.Create(ff, ImageList.TopLayerItems[0].HTMLText.Font.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(ColorToARGB(ImageList.TopLayerItems[0].HTMLText.Font.Color));
      sri := MakeRect(AItemRect.X + 5, AItemREct.Y + AItemRect.Height - 35, AItemRect.Width - 16, 25);
      ImageList.ItemAppearance.Fill.BeginUpdate;
      ImageList.ItemAppearance.Fill.BorderOpacity := 50;
      ImageList.ItemAppearance.Fill.ShadowOffset := 0;
      ImageList.ItemAppearance.Fill.Fill(g, sri);
      ImageList.ItemAppearance.Fill.ShadowOffset := 5;
      ImageList.ItemAppearance.Fill.BorderOpacity := 255;
      ImageList.ItemAppearance.Fill.EndUpdate;
      sri := MakeRect(sri.X + 2, sri.Y + 2, sri.Width - 4, sri.Height - 4);
      g.DrawString(Caption.Text, Length(Caption.Text), f, sri, sf, b);

      b.Free;
      f.Free;
      sf.Free;
      ff.Free;
    end;
  end;
end;

procedure TGDIPPictureContainerEditor.ImageListItemSelect(Sender: TObject;
  itemindex: Integer);
begin
  if (itemindex >= 0) and (itemindex <= ImageList.Items.Count -1) then
    Button3.Enabled := ImageList.Items[itemindex].Tag = 0;
end;

procedure TGDIPPictureContainerEditor.ImageListItemStartDrag(Sender: TObject;
  ItemIndex: Integer; var AllowDrag: Boolean);
begin
  AllowDrag := ImageList.Items[Itemindex].Tag = 0;
end;

procedure TGDIPPictureContainerEditor.ImageListItemZoomIn(Sender: TObject;
  itemindex: Integer; var allow: Boolean);
begin
  Allow := ImageList.Items[Itemindex].Tag = 0;
end;

procedure TGDIPPictureContainerEditor.ImageListResize(Sender: TObject);
begin
  if CheckBox1.Checked then
    ImageList.Rows := Max(ImageList.Height div 150, 1);
end;

procedure TGDIPPictureContainerEditor.Init;
var
  I: Integer;
begin
  PopulateDirectory(AddBackSlash(PathToMyPictures)+'*.*');
  for I := 0 to PictureCol.Count - 1 do
  begin
    with FileList.Items.Add do
    begin
      Caption.Text := PictureCol[i].Name;
      Image.Assign(PictureCol[i].Picture);
    end;
  end;

  ImageList.SelectedItemIndex := -1;

  button4.Enabled := FileList.Items.Count > 0;
  button2.Enabled := (FileList.SelectedItemIndex <> -1) and (FileList.Items.Count > 0);

  {$IFDEF DELPHI2006_LVL}
  ImageList.ItemAppearance.ImageMargin.Right := 8;
  ImageList.ItemAppearance.ImageMargin.Bottom := 8;
  FileList.ItemAppearance.ImageMargin.Right := 8;
  FileList.ItemAppearance.ImageMargin.Bottom := 8;
  {$ENDIF}
end;

procedure TGDIPPictureContainerEditor.PopulateDirectory(AFolder: string);
var
  SR: TSearchRec;
  par, f: String;
  AddItem: boolean;
  i: integer;

  procedure LoadFromRes(resname: string; picture: TAdvGDIPPicture);
  var
    rs: TResourceStream;
  begin
    rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
    picture.LoadFromStream(rs);
    rs.Free;
  end;

  procedure AddToList(s: string; Attr: integer);
  begin
    if not Attr and faHidden = faHidden then
    begin
      if Attr and faDirectory = faDirectory then
      begin
        if (ExtractFileName(s) <> '.') then
        begin
          AddItem := true;
          if (pos('..',s) > 0) then
          begin
            f := ExtractFilepath(s);
            par := GetParentDirectory(f);
            AddItem := par <> f;
          end;

          if AddItem then
          begin
            with ImageList.Items.Add do
            begin
              if (ExtractFileName(s) = '..') then
              begin
                FileName := GetParentDirectory(f);
                Caption.Text :=  ExtractFileName(FileName);
                if Caption.Text = '' then
                  Caption.Text := FileName;

                LoadFromRes('folderup',Image);
              end
              else
              begin
                FileName := s;
                Caption.Text := ExtractFileName(s);
                LoadFromRes('folder',Image);
              end;
              Tag := Attr;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  Screen.Cursor := crHourGlass;
  CurrentDir := ExtractFilePath(AFolder);
  ImageList.Footer.Caption := 'Current directory : ' + Currentdir;
  ImageList.Items.Clear;
  ImageList.Items.BeginUpdate;
  if FindFirst(AFolder,faAnyFile,SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name, SR.Attr);
    while FindNext(SR) = 0 do
    begin
      AddToList(ExtractFilePath(AFolder) + SR.Name, SR.Attr);
    end;
  end;
  FindClose(SR);
  case ComboBox2.ItemIndex of
  0:
  begin
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.jpg', true);
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.jpeg', true);
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.png', true);
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.bmp', true);
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.gif', true);
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.tiff', true);
    ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.ico', true);
  end;
  1: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.bmp', true);
  2: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.jpg', true);
  3: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.jpeg', true);
  4: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.png', true);
  5: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.gif', true);
  6: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.tiff', true);
  7: ImageList.AddImageLocationsFromFolder(ExtractFilePath(AFolder)+'\*.ico', true);
  end;

  for I := 0 to ImageList.Items.Count - 1 do
  begin
    with ImageList.Items[i] do
    begin
      if Tag = 0 then
      begin
        Hint := 'Select image and drag drop image in picture list to add to the collection' + #13#10;
        Hint := Hint + 'Double click on the image to zoom in' + #13#10 + #13#10;
        Hint := Hint + 'File Name : ' + FileName + #13#10 + 'File Date : ' + DateToStr(FileDate) + #13#10 + 'File Size : ' + FormatByteSize(FileSize);
      end
      else
      begin
        Hint := 'Click on the item to go to the directory' + #13#10 + #13#10 + 'Directory Name : '+ FileName;
      end
    end;
  end;


  ImageList.ItemAppearance.TextVisible := false;
  ImageList.Items.EndUpdate;
  Screen.Cursor := crDefault;
end;

procedure TGDIPPictureContainerEditor.TrackBar1Change(Sender: TObject);
begin
  ImageList.Rows := TrackBar1.Position;
end;

{ TGDIPPictureContainerEditorDialog }

function TGDIPPictureContainerEditorDialog.Execute: Boolean;
begin
  FForm := TGDIPPictureContainerEditor.Create(Application);

  if not Assigned(FPictureCol) then
  begin
    raise Exception.Create('The dialog does not have a TGDIPPictureContainer class assigned.');
    Result := False;
    Exit;
  end;

  if FCaption <> '' then
    Form.Caption := FCaption;

  try
    FForm.PictureCol := FPictureCol;
    FForm.Init;
    Result := FForm.ShowModal = mrOK;
  finally
    FForm.Free;
  end;
end;

procedure TGDIPPictureContainerEditorDialog.SetPictureCol(
  const Value: TPictureCollection);
begin
  FPictureCol := Value;
end;

end.
