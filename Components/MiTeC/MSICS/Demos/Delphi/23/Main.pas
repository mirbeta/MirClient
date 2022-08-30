unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TwndMain = class(TForm)
    List: TListView;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  wndMain: TwndMain;

implementation

uses ActiveX, MiTeC_Routines, MiTeC_TaskScheduler_TLB;

{$R *.dfm}

procedure TwndMain.FormCreate(Sender: TObject);
var
  i: Integer;
  tslist: TTSTasks;
begin
  GetTaskList(tslist);
  for i:=0 to High(tslist) do
    with List.Items.Add do begin
      Caption:=tslist[i].Path;
      SubItems.Add(tslist[i].ImagePath+' '+tslist[i].Args);
      SubItems.Add(tslist[i].Author);
    end;
end;

end.
