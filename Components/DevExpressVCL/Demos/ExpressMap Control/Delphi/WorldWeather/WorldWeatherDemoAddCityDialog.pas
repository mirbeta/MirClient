unit WorldWeatherDemoAddCityDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, Menus, StdCtrls,
  cxButtons, cxTextEdit;

type
  TWorldWeatherDemoAddCityDialogForm = class(TForm)
    cxTextEdit1: TcxTextEdit;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
  private
    function GetCityName: string;
    { Private declarations }
  public
    function Execute: Boolean;
    property CityName: string read GetCityName;
  end;

var
  WorldWeatherDemoAddCityDialogForm: TWorldWeatherDemoAddCityDialogForm;

implementation

{$R *.dfm}

{ TWorldWeatherDemoAddCityDialogForm }

function TWorldWeatherDemoAddCityDialogForm.Execute: Boolean;
begin
  Result := WorldWeatherDemoAddCityDialogForm.ShowModal = mrOk;
end;

function TWorldWeatherDemoAddCityDialogForm.GetCityName: string;
begin
  Result := cxTextEdit1.Text;
end;

end.
