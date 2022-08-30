{***************************************************************************}
{ TPictureContainer property editor                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2013                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit PictureContainerProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PictureContainer, HTMListB, ShellAPi, ExtDlgs;

type
  TContainerEditor = class(TForm)
    PictureContainer: TPictureContainer;
    HTMListBox: THTMListBox;
    AddBtn: TButton;
    RemoveBtn: TButton;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    ChangeName: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LBWindowProc(var Message: TMessage);
    procedure WMDROPFILES(var Msg: TMessage);
    procedure FormDestroy(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure ChangeNameClick(Sender: TObject);
    procedure HTMListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HTMListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetPictureInList(Index: Integer; APictureItem: TPictureItem);
    procedure UpdateList;
  end;

var
  ContainerEditor: TContainerEditor;

var
  OldLBWindowProc: TWndMethod;


implementation

{$R *.DFM}

procedure TContainerEditor.WMDROPFILES(var Msg: TMessage);
var
  pcFileName: PChar;
  i, iSize, iFileCount,h,w: integer;
  pcFileExt: string;
begin
  pcFileName := ''; // to avoid compiler warning message
  iFileCount := DragQueryFile(Msg.WParam, $FFFFFFFF, pcFileName, 255);
  for i := 0 to iFileCount - 1 do
  begin
    iSize := DragQueryFile(Msg.wParam, 0, nil, 0) + 1;
    pcFileName := StrAlloc(iSize);
    DragQueryFile(Msg.WParam, i, pcFileName, iSize);
    if FileExists(pcFileName) then
    begin
      pcFileExt := Uppercase(ExtractFileExt(pcFilename));

      if (pcFileExt = '.GIF') or (pcFileExt = '.JPG') or (pcFileExt = '.JPEG') or
         (pcFileExt = '.WMF') or (pcFileExt = '.ICO') or (pcFileExt = '.BMP') then
      with PictureContainer.Items.Add do
      begin
        Picture.LoadFromFile(pcFilename);
        Name := ExtractFileName(pcFileName);
        HTMListbox.BeginUpdate;
        if (Picture.Width > 100) or (Picture.Height > 100) then
        begin
          w := 100;
          h := Round(w/Picture.Width*Picture.Height);
          if h > 100 then
          begin
            h := 100;
            w := Round(h/Picture.Height*Picture.Width);
          end;
          HTMListBox.Items.Add('<p align="center"><IMG src="'+Name+'" width="'+inttostr(w)+'" height="'+inttostr(h)+'"></p>'+Name);
        end
        else
        begin
          HTMListBox.Items.Add('<p align="center"><IMG src="'+Name+'"></p>'+Name);
        end;
        HTMListbox.EndUpdate;

      end;

    end;
    StrDispose(pcFileName);
  end;
  DragFinish(Msg.WParam);
end;

procedure TContainerEditor.LBWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DROPFILES then
    WMDROPFILES(Message); // handle WM_DROPFILES message
  OldLBWindowProc(Message);  // call default ListBox1 WindowProc method to handle all other messages
end;



procedure TContainerEditor.FormCreate(Sender: TObject);
begin
  OldLBWindowProc := HTMListBox.WindowProc;
  HTMListBox.WindowProc := LBWindowProc;
  DragAcceptFiles(HTMListBox.Handle, True);
end;

procedure TContainerEditor.FormDestroy(Sender: TObject);
begin
  HTMListBox.WindowProc := OldLBWindowProc;
  DragAcceptFiles(HTMListBox.Handle, False);
end;

procedure TContainerEditor.AddBtnClick(Sender: TObject);
var
  idx: Integer;
  APictureItem: TPictureItem;

begin
  if OpenDialog.Execute then
  begin
    APictureItem := PictureContainer.Items.Add;

    APictureItem.Picture.LoadFromFile(OpenDialog.Filename);
    APictureItem.Name := ExtractFileName(OpenDialog.FileName);

    idx := HTMListBox.Items.Add('');

    SetPictureInList(idx,APictureItem);
  end;
end;

procedure TContainerEditor.RemoveBtnClick(Sender: TObject);
begin
  if HTMListBox.ItemIndex >= 0 then
  begin
    PictureContainer.Items.Items[HTMListBox.ItemIndex].Free;
    HTMListBox.Items.Delete(HTMListBox.ItemIndex);
  end;
end;

procedure TContainerEditor.ChangeNameClick(Sender: TObject);
var
  s: string;
  idx: Integer;
begin
  idx := HTMListBox.ItemIndex;
  if idx >= 0 then
    s := PictureContainer.Items.Items[idx].Name
  else
    Exit;

  if InputQuery('Picture name','Name',s) then
  begin
    with PictureContainer.Items.Items[idx] do
    begin
      Name := s;
      SetPictureInList(idx, PictureContainer.Items.Items[idx]);
    end;
  end;
end;

procedure TContainerEditor.SetPictureInList(Index: Integer; APictureItem: TPictureItem);
var
  w,h: Integer;
begin
  with APictureItem do
  begin
    if (Picture.Width > 100) or (Picture.Height > 100) then
    begin
      w := 100;
      h := Round(w/Picture.Width*Picture.Height);
      if h > 100 then
      begin
        h := 100;
        w := Round(h/Picture.Height*Picture.Width);
      end;
      HTMListBox.Items[Index] := '<p align="center"><IMG src="'+Name+'" width="'+inttostr(w)+'" height="'+inttostr(h)+'"></p>'+Name;
    end
    else
    begin
      HTMListBox.Items[Index] := '<p align="center"><IMG src="'+Name+'"></p>'+Name;
    end;
  end;
end;

procedure TContainerEditor.UpdateList;
var
  i,idx: Integer;
begin
  for i := 1 to PictureContainer.Items.Count do
  begin
    idx := HTMListBox.Items.Add('');
    SetPictureInList(idx,PictureContainer.Items.Items[i - 1]);
  end;
end;

procedure TContainerEditor.HTMListBoxDblClick(Sender: TObject);
begin
  ChangeNameClick(Sender);
end;

procedure TContainerEditor.HTMListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    RemoveBtnClick(Sender);

  if Key = VK_F2 then
    ChangeNameClick(Sender);
end;

end.
