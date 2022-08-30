{********************************************************************}
{ TAdvRangeSlider component                                          }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2012                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvRangeSliderGallery;

interface

{$R ADVRANGESLIDERGALLERY.RES}

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvTrackBar, AdvTrackBarPersist;

type
  TAdvRangeSliderGalleryForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    AdvRangeSlider1: TAdvRangeSlider;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    FSelection: string;
    procedure ResToFile(ResName,FileName: string);
  public
    { Public declarations }
    procedure LoadList;
    property Selection: string read FSelection write FSelection;
  end;

var
  AdvRangeSliderGalleryForm: TAdvRangeSliderGalleryForm;

implementation

uses
  ShlObj, ActiveX, ShellAPI;

{$R *.dfm}

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    Exit;
//    raise Exception.Create('Cannot create directory');

  if Dir[length(Dir)] = '\' then
    Delete(Dir,length(Dir),1);

  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

function WinTempDir: string;
var
  buf:string;
  i: integer;
begin
  SetLength(buf, MAX_PATH);
  i := GetTempPath(Length(buf), PChar(buf));
  SetLength(buf, i);
  Result := AddBackslash(buf);
end;

{ get Application data folder }

{$IFDEF DELPHI6_LVL}
procedure FreePidl( pidl: PItemIDList );
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;

function GetAppData: string;
var
  pidl: PItemIDList;
  Path: array [0..MAX_PATH-1] of char;
begin
  Result := '';

  if Succeeded(
       SHGetSpecialFolderLocation(0, CSIDL_APPDATA, pidl)
     ) then
  begin
    if SHGetPathFromIDList(pidl, Path) then
      Result := AddBackSlash(StrPas(Path));
    FreePidl(pidl);
    ForceDirectories(Result + 'tmssoftware');
  end;
end;
{$ENDIF}

{$IFNDEF DELPHI6_LVL}
function GetAppData: string;
begin
  result := WinTempDir;
end;
{$ENDIF}



procedure TAdvRangeSliderGalleryForm.ListBox1Click(Sender: TObject);
var
  pp: TPropertyPersister;
begin
  if listbox1.ItemIndex < 0 then
    Exit;
  FSelection := GetAppData + 'tmssoftware\'+ listbox1.items[listbox1.itemIndex]+'.RSPROP';

  AdvRangeSlider1.Free;

  AdvRangeSlider1 := TAdvRangeSlider.Create(self);
  AdvRangeSlider1.Parent := GroupBox1;
  AdvRangeSlider1.Left := 10;
  AdvRangeSlider1.Top := 20;

  pp := TPropertyPersister.Create(self);
  pp.IgnoreSubProperties.Add('ColorRemaining');
  pp.IgnoreSubProperties.Add('ColorRemainingTo');
  pp.IgnoreSubProperties.Add('ColorRemainingDisabled');
  pp.IgnoreSubProperties.Add('ColorRemainingDisabledTo');
  pp.IgnoreSubProperties.Add('PictureRemaining');
  pp.IgnoreSubProperties.Add('PictureRemainingDisabled');

  pp.ReStorePropertiesToFile(AdvRangeSlider1, FSelection);
  pp.Free;
end;

{$WARNINGS OFF}
procedure TAdvRangeSliderGalleryForm.LoadList;
var
  SR: TSearchRec;
  FileAttrs: Integer;
  len: integer;
begin
  Listbox1.Items.Clear;

  // get gallery files
  FileAttrs := faArchive;
  if FindFirst(GetAppData + 'tmssoftware\*.RSPROP',FileAttrs,SR) = 0 then
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
{$WARNINGS ON}



procedure TAdvRangeSliderGalleryForm.Button1Click(Sender: TObject);
var
  pp: TPropertyPersister;
  fname: string;
begin
  if Edit1.Text = '' then
  begin
    ShowMessage('Please specify a name for saving to gallery');
    Exit;
  end;

  if ListBox1.Items.IndexOf(edit1.Text) <> -1 then
  begin
    if MessageDlg('Name already exists. Are you sure to overwrite ?',mtWarning,[mbYes,mbNo],0) = mrNo then
      Exit;
  end;

  fname := GetAppData + 'tmssoftware\'+ Edit1.Text + '.RSPROP';

  pp := TPropertyPersister.Create(self);
  pp.IgnoreSubProperties.Add('ColorRemaining');
  pp.IgnoreSubProperties.Add('ColorRemainingTo');
  pp.IgnoreSubProperties.Add('ColorRemainingDisabled');
  pp.IgnoreSubProperties.Add('ColorRemainingDisabledTo');
  pp.IgnoreSubProperties.Add('PictureRemaining');
  pp.IgnoreSubProperties.Add('PictureRemainingDisabled');

  pp.StorePropertiesToFile(AdvRangeSlider1,fname);
  pp.Free;

  LoadList;
end;

procedure TAdvRangeSliderGalleryForm.ResToFile(ResName,FileName: string);
var
  reshandle: THandle;
  hglobal: THandle;
  ressize: dword;
  ptr: pointer;
  rtext: string;
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
    rtext := rtext + char(ptr^);
    ptr := pointer(LParam(ptr) + 1);
  end;

  {$IFDEF DELPHI6_LVL}
  AssignFile(tf, GetAppData + 'tmssoftware\'+FileName+'.RSPROP');
  {$ELSE}
  AssignFile(tf,'.\'+FileName+'.RSPROP');
  {$ENDIF}
  {$i-}
  rewrite(tf);
  {$i+}
  if ioresult = 0 then
  begin
    write(tf,rtext);
    CloseFile(tf);
  end;
end;


procedure TAdvRangeSliderGalleryForm.FormCreate(Sender: TObject);
var
  pp: TPropertyPersister;
begin
  pp := TPropertyPersister.Create(self);
  pp.IgnoreSubProperties.Add('ColorRemaining');
  pp.IgnoreSubProperties.Add('ColorRemainingTo');
  pp.IgnoreSubProperties.Add('ColorRemainingDisabled');
  pp.IgnoreSubProperties.Add('ColorRemainingDisabledTo');
  pp.IgnoreSubProperties.Add('PictureRemaining');
  pp.IgnoreSubProperties.Add('PictureRemainingDisabled');

  pp.ReStorePropertiesToFile(AdvRangeSlider1,WinTempDir + 'temp.prop');
  pp.Free;
  LoadList;

  if ListBox1.Items.Count = 0 then //
  begin
    // extract resources

    ResToFile('RS1','Airco');
    ResToFile('RS2','Apple');
    ResToFile('RS3','Circular');
    ResToFile('RS4','iPhone');
    ResToFile('RS5','iTunes');
    ResToFile('RS6','Mixer');
    ResToFile('RS7','Space');
    ResToFile('RS8','Office 2007 Luna');
    ResToFile('RS9','Windows Media Player 10 Seek');
    ResToFile('RS10','Windows Media Player 10');
    ResToFile('RS11','Windows Media Player 11');
    
    LoadList;
  end;
end;

end.
