        ??  ??                    8   ??
 C O D E G E N S T Y L E         0         unit %0:s;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvToolBar, AdvStyleIF;

type
  T%1:s = class(%2:s)
  procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  end;

var
  %1:s: T%1:s;

implementation

{$R *.dfm}

procedure T%1:s.FormCreate(Sender: TObject);
begin
  SetComponentStyle(tsOffice2007Luna);
end;


end.
 