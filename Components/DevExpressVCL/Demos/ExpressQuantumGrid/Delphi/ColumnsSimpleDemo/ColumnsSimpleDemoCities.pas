unit ColumnsSimpleDemoCities;

{$I cxVer.inc}

interface

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  StdCtrls, cxButtons, cxGridLevel, cxControls,
  cxGridCustomView, cxGrid, ExtCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, cxEdit, DB, cxDBData, cxClasses, cxLookAndFeelPainters,
  cxDataStorage, Menus;

type
  TColumnsSimpleDemoCitiesForm = class(TForm)
    pnlCustomize: TPanel;
    tvCities: TcxGridDBTableView;
    lvCities: TcxGridLevel;
    GridCities: TcxGrid;
    btnSet: TcxButton;
    btnAdd: TcxButton;
    btnDelete: TcxButton;
    btnCancel: TcxButton;
    tvCitiesCity: TcxGridDBColumn;
    lbDescription: TLabel;
    procedure tvCitiesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    function GetValue: Variant;
    { Private declarations }
  public
    { Public declarations }
    property Value: Variant read GetValue;
  end;

var
  ColumnsSimpleDemoCitiesForm: TColumnsSimpleDemoCitiesForm;

implementation

uses ColumnsSimpleDemoData;

{$R *.dfm}

function TColumnsSimpleDemoCitiesForm.GetValue: Variant;
begin
  Result := tvCities.Controller.FocusedRecord.Values[0];
end;

procedure TColumnsSimpleDemoCitiesForm.tvCitiesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not tvCities.Controller.IsEditing then
    ModalResult := mrOk;
end;

procedure TColumnsSimpleDemoCitiesForm.btnAddClick(Sender: TObject);
begin
  tvCities.DataController.Insert;
  tvCities.DataController.Edit;
end;

procedure TColumnsSimpleDemoCitiesForm.btnDeleteClick(Sender: TObject);
begin
  tvCities.Controller.DeleteSelection;
end;

end.

