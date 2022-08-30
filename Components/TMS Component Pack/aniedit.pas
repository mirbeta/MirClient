{**********************************************************
ANIEDIT property editor for AniFiles
for Delphi 2.0, 3.0, 4.0 & C++Builder 1,3,4
version 2.0

written
  TMS Software
  copyright © 1998-1999
  Email : info@tmssoftware.com
  Web : http://www.tmssoftware.com

The source code is given as is. The author is not responsible
for any possible damage done due to the use of this code.
The component can be freely used in any application. The complete
source code remains property of the author and may not be distributed,
published, given or sold in any form as such. No parts of the source
code can be included in any other component or application without
written authorization of the author.
********************************************************************}
unit AniEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  aniicon, StdCtrls, ExtCtrls;

type
  TAniEditor = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    AniIcon: TAniIcon;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FileName:tFilename;
    { Public declarations }
  end;

var
  AniEditor: TAniEditor;

implementation

{$R *.DFM}

procedure TAniEditor.FormCreate(Sender: TObject);
begin
 FileName:='';
end;

procedure TAniEditor.Button3Click(Sender: TObject);
begin
 if OpenDialog1.Execute then
   begin
    FileName:=Opendialog1.FileName;
    AniIcon.AniFile.LoadFromFile(FileName);
   end;
end;

procedure TAniEditor.Button2Click(Sender: TObject);
begin
 AniIcon.Animated:=false;
end;

procedure TAniEditor.Button1Click(Sender: TObject);
begin
 AniIcon.Animated:=true;
end;

end.
