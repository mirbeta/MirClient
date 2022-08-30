unit EditorsInPlaceDemoCars;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView,
  cxGridDBBandedTableView, cxControls, cxGridCustomView,
  cxGridLevel, cxGrid, Grids, DBGrids, ExtCtrls, cxStyles, cxCustomData,
  cxGraphics, cxFilter, cxData, cxEdit, cxClasses, StdCtrls,
  cxDropDownEdit, cxDBEdit, cxMemo, cxCheckBox, cxMaskEdit, cxSpinEdit,
  cxContainer, cxTextEdit, cxImageComboBox, cxListBox, DBCtrls, cxLookAndFeels,
  cxLookAndFeelPainters, cxGroupBox, cxLabel, CarsDataForGrid, cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox;

type
  TEditorsInPlaceDemoCarsForm = class(TForm)
    pnlCarInfo: TcxGroupBox;
    cxDBMemo1: TcxDBMemo;
    Panel1: TcxGroupBox;
    cxDBTextEdit1: TcxDBTextEdit;
    cxDBTextEdit2: TcxDBTextEdit;
    Label3: TcxLabel;
    Label4: TcxLabel;
    GroupBox1: TcxGroupBox;
    cxDBTextEdit5: TcxDBTextEdit;
    cxDBTextEdit4: TcxDBTextEdit;
    cxDBTextEdit8: TcxDBTextEdit;
    GroupBox2: TcxGroupBox;
    cxDBCheckBox: TcxDBCheckBox;
    Label6: TcxLabel;
    GroupBox3: TcxGroupBox;
    cxDBTextEdit7: TcxDBTextEdit;
    cxDBTextEdit6: TcxDBTextEdit;
    cxDBListBox1: TcxDBListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditorsInPlaceDemoCarsForm: TEditorsInPlaceDemoCarsForm;

implementation

uses EditorsInPlaceDemoData;

{$R *.dfm}

end.
