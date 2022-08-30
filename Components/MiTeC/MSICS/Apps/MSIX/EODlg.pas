{$INCLUDE ..\..\Compilers.inc}

unit EODlg;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls, VCL.Buttons;
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls, Buttons;
     {$ENDIF}

type
  Tdlg_msi_EO = class(TForm)
    Button1: TButton;
    Panel: TPanel;
    Memo: TMemo;
    GroupHeader: TPanel;
    Icon: TImage;
    lMachine: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  dlg_msi_EO: Tdlg_msi_EO;

implementation

{$R *.dfm}

procedure Tdlg_msi_EO.FormCreate(Sender: TObject);
begin
  Icon.Picture.Icon.Handle:=Application.Icon.Handle;
  {$IFDEF THEMESUPPORT}
  GroupHeader.ParentBackground:=False;
  {$ENDIF}
end;

end.
