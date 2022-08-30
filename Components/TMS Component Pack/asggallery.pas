{ *************************************************************************** }
{ TAdvStringGrid gallery component                                            }
{ for Delphi & C++Builder                                                     }
{                                                                             }
{ written by TMS Software                                                     }
{ copyright © 1996 - 20013                                                    }
{ Email : info@tmssoftware.com                                                }
{ Web : http://www.tmssoftware.com                                            }
{                                                                             }
{ The source code is given as is. The author is not responsible               }
{ for any possible damage done due to the use of this code.                   }
{ The component can be freely used in any application. The complete           }
{ source code remains property of the author and may not be distributed,      }
{ published, given or sold in any form as such. No parts of the source        }
{ code can be included in any other component or application without          }
{ written authorization of the author.                                        }
{ *************************************************************************** }

unit AsgGallery;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, StdCtrls, AdvObj;

{$R GALLERY.RES}
{$I TMSDEFS.INC}

type
  TGridGallery = class(TForm)
    GroupBox1: TGroupBox;
    AdvStringGrid1: TAdvStringGrid;
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    FGalleryFile: string;
  public
    { Public declarations }
    oldidx: integer;
    property GalleryFile: string read FGalleryFile;
    procedure InitGrid;
    procedure LoadList;
    procedure ResToFile(ResName, FileName: string);
  end;

var
  GridGallery: TGridGallery;

implementation

uses
  ShlObj, ActiveX, ShellAPI;

{$R *.dfm}

{ Get My Documents folder }

procedure FreePidl(pidl: PItemIDList);
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;

function GetMyDocuments: string;
var
  pidl: PItemIDList;
  Path: array [0 .. MAX_PATH - 1] of char;
begin
  Result := '';

  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, pidl)) then
  begin
    if SHGetPathFromIDList(pidl, Path) then
      Result := StrPas(Path);
    FreePidl(pidl);
  end;
end;

procedure TGridGallery.InitGrid;
var
  i: integer;
begin
  AdvStringGrid1.RowCount := 7;
  AdvStringGrid1.ColCount := 7;
  AdvStringGrid1.LinearFill(false);
  AdvStringGrid1.Row := 4;
  AdvStringGrid1.ColWidths[0] := 40;

  for i := 1 to 3 do
    AdvStringGrid1.Cells[1, i] := 'Group 1';

  for i := 1 to 3 do
    AdvStringGrid1.Cells[1, 3 + i] := 'Group 2';

  AdvStringGrid1.Grouping.Summary := true;
  AdvStringGrid1.Grouping.MergeHeader := true;
  AdvStringGrid1.Grouping.MergeSummary := true;
  AdvStringGrid1.Grouping.MergeSummary := true;

  AdvStringGrid1.Group(1);

  AdvStringGrid1.AddComment(1, 2, 'A comment');
  AdvStringGrid1.ShowHint := true;

  AdvStringGrid1.SortSettings.Column := 1;
  AdvStringGrid1.SortSettings.Show := true;
  AdvStringGrid1.SortSettings.IndexShow := true;
  AdvStringGrid1.SortIndexes.Add(1);
  AdvStringGrid1.QSortGroupIndexed;

  AdvStringGrid1.SearchFooter.ShowMatchCase := false;
  AdvStringGrid1.SearchFooter.ShowHighLight := false;
  AdvStringGrid1.SearchFooter.Visible := true;

  AdvStringGrid1.Cells[1, 0] := 'A';
  AdvStringGrid1.Cells[2, 0] := 'B';
  AdvStringGrid1.Cells[3, 0] := 'C';
  AdvStringGrid1.Cells[4, 0] := 'D';
  AdvStringGrid1.Cells[5, 0] := 'E';

  AdvStringGrid1.Row := 4;
  AdvStringGrid1.Options := AdvStringGrid1.Options + [goRowSelect];
  AdvStringGrid1.Repaint;
  AdvStringGrid1.SearchPanel.Repaint;
  AdvStringGrid1.SearchPanel.EditControl.Width := AdvStringGrid1.SearchPanel.EditControl.Width + 1;
  AdvStringGrid1.SearchPanel.EditControl.Width := AdvStringGrid1.SearchPanel.EditControl.Width - 1;
end;

procedure TGridGallery.ResToFile(ResName, FileName: string);
var
  reshandle: THandle;
  hglobal: THandle;
  ressize: dword;
  ptr: pointer;
  rtext: ansistring;
  tf: TextFile;
  i: integer;
begin
  reshandle := FindResource(hinstance, PChar(ResName), PChar(RT_RCDATA));
  hglobal := LoadResource(hinstance, reshandle);
  ressize := SizeOfResource(hinstance, reshandle);
  ptr := LockResource(hglobal);

  rtext := '';
  for i := 1 to ressize do
  begin
    rtext := rtext + ansichar(ptr^);
    ptr := pointer(integer(ptr) + 1);
  end;
  AssignFile(tf, GetMyDocuments + '\' + FileName + '.GP');
{$I-}
  rewrite(tf);
{$I+}
  if ioresult = 0 then
  begin
    write(tf, rtext);
    CloseFile(tf);
  end;
end;
{$WARNINGS OFF}

procedure TGridGallery.LoadList;
var
  SR: TSearchRec;
  FileAttrs: integer;
  len: integer;
begin
  // get gallery files
  FileAttrs := faArchive;
  if FindFirst(GetMyDocuments + '\*.GP', FileAttrs, SR) = 0 then
  begin
    repeat
      if (SR.Attr and FileAttrs) = FileAttrs then
      begin
        len := Length(ExtractFileExt(SR.Name));
        ListBox1.Items.Add(copy(SR.Name, 1, Length(SR.Name) - len));
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;
{$WARNINGS ON}

procedure TGridGallery.FormCreate(Sender: TObject);
begin
  InitGrid;
  LoadList;

  if (ListBox1.Items.Count < 15) then //
  begin
    // extract resources
    ResToFile('G1', 'Office 2003 Blue');
    ResToFile('G2', 'Office 2003 Olive');
    ResToFile('G3', 'Office 2003 Silver');
    ResToFile('G4', 'Whidbey');
    ResToFile('G5', 'Windows XP');
    ResToFile('G6', 'Arctic');
    ResToFile('G7', 'Aqua');
    ResToFile('G8', 'SilverFox');
    ResToFile('G9', 'MacOS');
    ResToFile('G10', 'BabyBlue');
    ResToFile('G11', 'SummerSand');
    ResToFile('G12', 'Classic');
    ResToFile('G13', 'Office 2007 Luna');
    ResToFile('G14', 'Office2007 Obsidian');
    ResToFile('G15', 'Office2007 Silver');
    ResToFile('G16', 'Office2010 Blue');
    ResToFile('G17', 'Office2010 Silver');
    ResToFile('G18', 'Office2010 Black');
    ResToFile('G19', 'Office2013 White');
    ResToFile('G20', 'Office2013 Light gray');
    ResToFile('G21', 'Office2013 Gray');
    ResToFile('G22', 'Windows 7');
    ResToFile('G23', 'Windows 8');

    LoadList;
  end;

  oldidx := -1;
end;

procedure TGridGallery.ListBox1Click(Sender: TObject);
var
  fName: string;
begin
  if (oldidx <> ListBox1.ItemIndex) and (ListBox1.ItemIndex <> -1) then
  begin
    fName := GetMyDocuments + '\' + ListBox1.Items[ListBox1.ItemIndex] + '.GP';
    FGalleryFile := fName;
    AdvStringGrid1.UnGroup;
    AdvStringGrid1.LoadVisualProps(fName);
    InitGrid;
    oldidx := ListBox1.ItemIndex;
  end;
end;

end.
