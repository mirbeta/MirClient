//---------------------------------------------------------------------------

#ifndef DragDropDemoDictionariesH
#define DragDropDemoDictionariesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCardView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TDragDropDemoDictionariesForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbDesc;
  TcxGrid *SourceGrid;
  TcxGridDBTableView *tvCompaniesList;
  TcxGridDBColumn *tvCompaniesListID;
  TcxGridDBColumn *tvCompaniesListCOMPANYTYPEID;
  TcxGridDBColumn *tvCompaniesListCOUNTRYID;
  TcxGridDBColumn *tvCompaniesListCOMPANYNAME;
  TcxGridDBColumn *tvCompaniesListCOMPANYWEBSITE;
  TcxGridDBTableView *tvFilmsList;
  TcxGridDBColumn *tvFilmsListID;
  TcxGridDBColumn *tvFilmsListCAPTION;
  TcxGridDBColumn *tvFilmsListYEAR;
  TcxGridDBColumn *tvFilmsListTAGLINE;
  TcxGridDBColumn *tvFilmsListPLOTOUTLINE;
  TcxGridDBColumn *tvFilmsListRUNTIME;
  TcxGridDBColumn *tvFilmsListCOLOR;
  TcxGridDBColumn *tvFilmsListPHOTO;
  TcxGridDBColumn *tvFilmsListICON;
  TcxGridDBColumn *tvFilmsListWEBSITE;
  TcxGridDBCardView *cvPersonsList;
  TcxGridDBCardViewRow *cvPersonsListID;
  TcxGridDBCardViewRow *cvPersonsListFIRSTNAME;
  TcxGridDBCardViewRow *cvPersonsListSECONDNAME;
  TcxGridDBCardViewRow *cvPersonsListGENDER;
  TcxGridDBCardViewRow *cvPersonsListBIRTHNAME;
  TcxGridDBCardViewRow *cvPersonsListDATEOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsListBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvPersonsListLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsListBIOGRAPHY;
  TcxGridDBCardViewRow *cvPersonsListNICKNAME;
  TcxGridDBCardViewRow *cvPersonsListHOMEPAGE;
  TcxGridLevel *glFilmsList;
  TcxGridLevel *glPersonsList;
  TcxGridLevel *glCompaniesList;
private:	// User declarations
public:		// User declarations
  __fastcall TDragDropDemoDictionariesForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDragDropDemoDictionariesForm *DragDropDemoDictionariesForm;
//---------------------------------------------------------------------------
#endif
 