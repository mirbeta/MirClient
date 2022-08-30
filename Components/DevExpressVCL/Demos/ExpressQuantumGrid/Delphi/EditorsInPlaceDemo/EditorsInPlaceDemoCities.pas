unit EditorsInPlaceDemoCities;

interface

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  StdCtrls, cxButtons, cxGridLevel, cxControls,
  cxGridCustomView, cxGrid, ExtCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, cxEdit, DB, cxDBData, cxClasses, EditorsInPlaceDemoData,
  cxDataStorage, cxLookAndFeelPainters;

type
  TEditorsInPlaceDemoCitiesForm = class(TForm)
    tvCities: TcxGridDBTableView;
    lvCities: TcxGridLevel;
    GridCities: TcxGrid;
    tvCitiesCity: TcxGridDBColumn;
    lbDescription: TLabel;
    btnCancel: TcxButton;
    btnOK: TcxButton;
    procedure tvCitiesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    function GetValue: Variant;
  public
    property Value: Variant read GetValue;
  end;

var
  EditorsInPlaceDemoCitiesForm: TEditorsInPlaceDemoCitiesForm;

implementation

{$R *.dfm}

function TEditorsInPlaceDemoCitiesForm.GetValue: Variant;
begin
  Result := tvCities.Controller.FocusedRecord.Values[0];
end;

procedure TEditorsInPlaceDemoCitiesForm.tvCitiesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not tvCities.Controller.IsEditing then
    ModalResult := mrOk;
end;

procedure TEditorsInPlaceDemoCitiesForm.btnAddClick(Sender: TObject);
begin
  tvCities.DataController.Insert;
  tvCities.DataController.Edit;
end;

procedure TEditorsInPlaceDemoCitiesForm.btnDeleteClick(Sender: TObject);
begin
  tvCities.Controller.DeleteSelection;
end;

end.

