{$INCLUDE ..\..\..\Compilers.inc}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_Security, ComCtrls, ExtCtrls;

type
  TwndMain = class(TForm)
    pAV: TPanel;
    lvAV: TListView;
    pAS: TPanel;
    lvAS: TListView;
    pFW: TPanel;
    lvFW: TListView;
    procedure FormCreate(Sender: TObject);
  private
    SC: TMiTeC_Security;
  public
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_StrUtils;

{$R *.lfm}

procedure TwndMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  {$IFDEF THEMESUPPORT}
  pAV.ParentBackground:=False;
  pAS.ParentBackground:=False;
  pFW.ParentBackground:=False;
  {$ENDIF}

  SC:=TMiTeC_Security.Create(Self);
  SC.RefreshData;
  for i:=0 to SC.AntiVirus.Count-1 do
    with lvAV.Items.Add do begin
      Caption:=ListName(SC.AntiVirus,i);
      SubItems.Add(ListValueFromIndex(SC.AntiVirus,i));
    end;
  for i:=0 to SC.AntiSpyware.Count-1 do
    with lvAS.Items.Add do begin
      Caption:=ListName(SC.AntiSpyware,i);
      SubItems.Add(ListValueFromIndex(SC.AntiSpyware,i));
    end;
  for i:=0 to SC.Firewall.Count-1 do
    with lvFW.Items.Add do begin
      Caption:=ListName(SC.Firewall,i);
      SubItems.Add(ListValueFromIndex(SC.Firewall,i));
    end;
end;

end.
