unit AdvSmoothMegaMenuGallery;

interface

{$R MEGAMENUGALLERY.RES}

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AdvSmoothMegaMenu, GDIPMenu, AdvStyleIF, AdvGDIP
  ;

type
  TCustomPaintBox = class(TPaintBox)
  private
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  end;

  TAdvSmoothMegaMenuGalleryForm = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    AdvSmoothMegaMenu1: TAdvSmoothMegaMenu;
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox1Click(Sender: TObject);
  private
    FGalleryFile: string;
    procedure MenuChanged(Sender: TObject);
  public
    { Public declarations }
    PaintBox1: TCustomPaintBox;
    oldidx: integer;
    procedure CalculateSize;
    property GalleryFile: string read FGalleryFile;
    procedure LoadList;
    procedure ResToFile(ResName,FileName:string);
    procedure Init;
  end;

var
  AdvSmoothMegaMenuGalleryForm: TAdvSmoothMegaMenuGalleryForm;
  m: TGDIPMenu;

implementation

uses
  ShlObj, ActiveX, ShellAPI;

{$R *.dfm}

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}


{ get My Documents folder }

procedure FreePidl( pidl: PItemIDList );
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;

function GetMyDocuments: string;
var
  pidl: PItemIDList;
  Path: array [0..MAX_PATH-1] of char;
begin
  Result := '';

  if Succeeded(
       SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, pidl)
     ) then
  begin
    if SHGetPathFromIDList(pidl, Path) then
      Result := StrPas(Path);
    FreePidl(pidl);
  end;
end;

procedure TAdvSmoothMegaMenuGalleryForm.Button1Click(Sender: TObject);
begin
  //
end;

procedure TAdvSmoothMegaMenuGalleryForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TAdvSmoothMegaMenuGalleryForm.CalculateSize;
var
  i: integer;
  maxh, maxw: integer;
begin
  maxh := 0;
  maxw := 0;
  with m do
  begin
    for I := 0 to m.Sections.Count - 1 do
    begin
      case m.SectionLayout of
        slHorizontal:
        begin
          if m.Sections[I].GetHeight > maxh then
            maxh := Round(m.Sections[I].GetHeight);

           maxw := maxw + Round(m.Sections[I].GetWidth);
        end;
        slVertical:
        begin
          if m.Sections[I].GetWidth > maxw then
            maxw := Round(m.Sections[I].GetWidth);

          maxh := maxh + Round(m.Sections[I].GetHeight);
        end;
      end;
    end;

    PaintBox1.Height := maxh + m.SectionMargin.Top + m.SectionMargin.Bottom;
    PaintBox1.Width := maxw + m.SectionMargin.Left + m.SectionMargin.Right;
    if m.ContentFill.BorderColor <> clNone then
    begin
      PaintBox1.Height := PaintBox1.Height + (m.ContentFill.BorderWidth * 2);
      PaintBox1.Width := PaintBox1.Width + (m.ContentFill.BorderWidth * 2);
    end;

    if m.ContentFill.ShadowColor <> clNone then
    begin
      PaintBox1.Height := PaintBox1.Height + m.ContentFill.ShadowOffset;
      PaintBox1.Width := PaintBox1.Width + m.ContentFill.ShadowOffset;
    end;

    if (m.TearOffSize > 0) and m.TearOff then
    begin
      PaintBox1.Height := PaintBox1.Height + m.TearOffSize;
    end;

    PaintBox1.Height := PaintBox1.Height + 1;
    PaintBox1.Width := PaintBox1.Width + 1;
  end;
end;

procedure TAdvSmoothMegaMenuGalleryForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  m.Free;
end;

procedure TAdvSmoothMegaMenuGalleryForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  PaintBox1 := TCustomPaintBox.Create(Self);
  PaintBox1.Parent := Self;
  PaintBox1.Left := 272;
  PaintBox1.Top := 58;
  PaintBox1.Width := 262;
  PaintBox1.Height := 193;
  PaintBox1.OnMouseMove := PaintBox1MouseMove;
  Paintbox1.OnMouseDown := Paintbox1MouseDown;
  PaintBox1.OnMouseUp := PaintBox1MouseUp;
  PaintBox1.OnPaint := PaintBox1Paint;

  m := TGDIPMenu.Create(Self);
  m.OnChange := MenuChanged;



  Init;
  m.SetComponentStyle(tsOffice2007Luna);
  CalculateSize;

  if ListBox1.Items.Count = 0 then //
  begin
    // extract resources
    ResToFile('Office2003Blue', 'Office 2003 Blue');
    ResToFile('Office2003Olive', 'Office 2003 Olive');
    ResToFile('Office2003Silver', 'Office 2003 Silver');
    ResToFile('Office2003Classic', 'Office 2003 Classic');
    ResToFile('Office2007Luna', 'Office 2007 Luna');
    ResToFile('Office2007Obsidian', 'Office 2007 Obsidian');
    ResToFile('Office2007Silver', 'Office 2007 Silver');
    ResToFile('CaveBlackStyle', 'Cave Black Style');
    ResToFile('OceanBlueStyle', 'Ocean Blue Style');
    ResToFile('WineRedStyle', 'Wine Red Style');
    ResToFile('FruitOrangeStyle', 'Fruit Orange Style');
    ResToFile('SpringGreenStyle', 'Spring Green Style');
    ResToFile('DarkBlueStyle', 'Dark Blue Style');
    ResToFile('DeepAutumnStyle', 'Deep Autumn Style');
    LoadList;
  end;
  oldidx := -1;
end;

procedure TAdvSmoothMegaMenuGalleryForm.Init;
var
  i, k: integer;
begin
  with m.Sections.Add do
  begin
    m.Sections[0].Caption := 'Section';
    m.Sections[0].Height := PaintBox1.Height;
    m.Sections[0].ItemMargin.Left := 5;
    m.Sections[0].ItemMargin.Top := 5;
    m.Sections[0].ItemMargin.Right := 5;
    m.Sections[0].ItemMargin.Bottom := 5;
    m.Sections[0].Width := PaintBox1.Width;
    m.AutoSectionSize := false;
    K := 1;
    for I := 0 to 14 do
    begin
      if I = 7 then
        m.Sections[0].Items.Add.ItemType := itBreak
      else
      begin
        m.Sections[0].Items.Add.Text := 'Item ' + inttostr(k);
        Inc(K);
      end;
    end;
  end;
  m.Init(PaintBox1.ClientRect, true, true);
end;

procedure TAdvSmoothMegaMenuGalleryForm.ListBox1Click(Sender: TObject);
var
  fName: string;
begin
  if (oldidx <> ListBox1.ItemIndex) and (Listbox1.ItemIndex <> -1) then
  begin
    {$IFDEF DELPHI6_LVL}
    fname := GetMyDocuments + '\' + ListBox1.Items[ListBox1.ItemIndex] + '.MMProp';
    {$ELSE}
    fname := '.\' + ListBox1.Items[ListBox1.ItemIndex] + '.MMProp';
    {$ENDIF}
    FGalleryFile := fname;

    AdvSmoothMegaMenu1.LoadFromFile(FGalleryFile, true);
    m.ContentFill.Assign(AdvSmoothMegaMenu1.DefaultMenuContentFill);
    m.Sections[0].BackGroundFill.Assign(AdvSmoothMegaMenu1.DefaultSection.BackGroundFill);
    m.Sections[0].CaptionFill.Assign(AdvSmoothMegaMenu1.DefaultSection.CaptionFill);
    m.Sections[0].CaptionFont.Assign(AdvSmoothMegaMenu1.DefaultSection.CaptionFont);
    m.ItemAppearance.Assign(AdvSmoothMegaMenu1.DefaultSectionItemAppearance);
    m.TearOffFill.Assign(AdvSmoothMegaMenu1.DefaultMenuTearOffFill);

    oldidx := ListBox1.ItemIndex;
  end;
end;

procedure TAdvSmoothMegaMenuGalleryForm.LoadList;
var
  SR: TSearchRec;
  FileAttrs: Integer;
  len: integer;
begin
  // get gallery files
  FileAttrs := faArchive;
  {$IFDEF DELPHI6_LVL}
  if FindFirst(GetMyDocuments+'\*.MMProp',FileAttrs,SR) = 0 then
  {$ELSE}
  if FindFirst('.\*.MMProp',FileAttrs,SR) = 0 then
  {$ENDIF}
  begin
    repeat
      if (sr.Attr and FileAttrs) = FileAttrs then
      begin
        len := Length(ExtractFileExt(sr.Name));
        listbox1.Items.Add(copy(sr.Name,1,length(sr.Name)-len));
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

end;

procedure TAdvSmoothMegaMenuGalleryForm.MenuChanged(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TAdvSmoothMegaMenuGalleryForm.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(m) then
    m.DoMouseDown(Button, Shift, X, Y);
end;

procedure TAdvSmoothMegaMenuGalleryForm.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(m) then
    m.DoMouseMove(Shift, X, Y);
end;

procedure TAdvSmoothMegaMenuGalleryForm.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(m) then
    m.DoMouseUp(Button, Shift, X, Y);
end;

procedure TAdvSmoothMegaMenuGalleryForm.PaintBox1Paint(Sender: TObject);
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(PaintBox1.Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  m.Draw(g, PaintBox1.ClientRect);
  g.Free;
end;

procedure TAdvSmoothMegaMenuGalleryForm.ResToFile(ResName, FileName: string);
var
  reshandle: THandle;
  hglobal: THandle;
  ressize: dword;
  ptr: pointer;
  rtext: AnsiString;
  tf: TextFile;
  i: Integer;
begin
  reshandle := FindResource(hinstance, PChar(ResName), PChar(RT_RCDATA));
  hglobal := LoadResource(hinstance, reshandle);
  Ressize := SizeOfResource(hinstance, reshandle);
  ptr := LockResource(hglobal);

  rtext := '';
  for i := 1 to ressize do
  begin
    rtext := rtext + AnsiChar(ptr^);
    ptr := pointer(LInteger(ptr) + 1);
  end;

  AssignFile(tf, GetMyDocuments+'\'+FileName+'.MMProp');
  {$i-}
  rewrite(tf);
  {$i+}
  if ioresult = 0 then
  begin
    write(tf,rtext);
    CloseFile(tf);
  end;
end;

{ TAdvSmoothMegaMenuGalleryDialog }

{ TCustomPaintBox }

procedure TCustomPaintBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(m) then
    m.DoCMMouseLeave(Msg);
end;

end.
