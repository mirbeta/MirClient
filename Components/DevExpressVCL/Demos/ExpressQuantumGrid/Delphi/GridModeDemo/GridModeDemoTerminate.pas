unit GridModeDemoTerminate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TGridModeDemoTerminateForm = class(TForm)
    Panel1: TPanel;
    lbDesc: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GridModeDemoTerminateForm: TGridModeDemoTerminateForm;

const
  strDeleting = 'Deleting previously inserted records...';
  strInserting = 'Inserting records...';
  strLoadData = 'Loading data ...';

implementation

{$R *.dfm}

end.
