unit DragDropDemoDictionaries;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxEdit,
  DB, cxDBData, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxClasses, cxControls, cxGridCustomView, cxGrid,
  cxGridCardView, cxGridDBCardView, FilmsDemoData, StdCtrls,
  cxDataStorage, cxDBLookupComboBox, cxBlobEdit, cxCheckBox,
  cxImageComboBox, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCustomLayoutView, Grids, DBGrids;

type
  TDragDropDemoDictionariesForm = class(TForm)
    SourceGrid: TcxGrid;
    tvCompaniesList: TcxGridDBTableView;
    tvCompaniesListID: TcxGridDBColumn;
    tvCompaniesListCOMPANYTYPEID: TcxGridDBColumn;
    tvCompaniesListCOUNTRYID: TcxGridDBColumn;
    tvCompaniesListCOMPANYNAME: TcxGridDBColumn;
    tvCompaniesListCOMPANYWEBSITE: TcxGridDBColumn;
    tvFilmsList: TcxGridDBTableView;
    tvFilmsListID: TcxGridDBColumn;
    tvFilmsListCAPTION: TcxGridDBColumn;
    tvFilmsListYEAR: TcxGridDBColumn;
    tvFilmsListTAGLINE: TcxGridDBColumn;
    tvFilmsListPLOTOUTLINE: TcxGridDBColumn;
    tvFilmsListRUNTIME: TcxGridDBColumn;
    tvFilmsListCOLOR: TcxGridDBColumn;
    tvFilmsListPHOTO: TcxGridDBColumn;
    tvFilmsListICON: TcxGridDBColumn;
    tvFilmsListWEBSITE: TcxGridDBColumn;
    glFilmsList: TcxGridLevel;
    glPersonsList: TcxGridLevel;
    glCompaniesList: TcxGridLevel;
    cvPersonsList: TcxGridDBCardView;
    cvPersonsListID: TcxGridDBCardViewRow;
    cvPersonsListFIRSTNAME: TcxGridDBCardViewRow;
    cvPersonsListSECONDNAME: TcxGridDBCardViewRow;
    cvPersonsListGENDER: TcxGridDBCardViewRow;
    cvPersonsListBIRTHNAME: TcxGridDBCardViewRow;
    cvPersonsListDATEOFBIRTH: TcxGridDBCardViewRow;
    cvPersonsListBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvPersonsListLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvPersonsListBIOGRAPHY: TcxGridDBCardViewRow;
    cvPersonsListNICKNAME: TcxGridDBCardViewRow;
    cvPersonsListHOMEPAGE: TcxGridDBCardViewRow;
    lbDesc: TLabel;
  private
  end;

var
  DragDropDemoDictionariesForm: TDragDropDemoDictionariesForm;

implementation

{$R *.dfm}

end.
