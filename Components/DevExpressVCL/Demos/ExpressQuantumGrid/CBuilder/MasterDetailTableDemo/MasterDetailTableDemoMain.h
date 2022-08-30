//---------------------------------------------------------------------------

#ifndef MasterDetailTableDemoMainH
#define MasterDetailTableDemoMainH
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
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ExtCtrls.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxContainer.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCardView.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TMasterDetailTableDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
	TcxLabel *lblStyle;
	TcxLabel *lblMaster;
  TBevel *Bevel1;
  TSplitter *Splitter;
  TPanel *pnlDetail;
	TcxLabel *lblDetail;
  TcxGrid *GridDetail;
  TcxGridDBTableView *tvFilmsPersonsStaff;
  TcxGridDBColumn *tvFilmsPersonsStaffPERSONLINEID;
  TcxGridDBColumn *tvFilmsPersonsStaffPERSONID;
  TcxGridDBColumn *tvFilmsPersonsStaffDESCRIPTION;
  TcxGridLevel *lvDetail;
  TcxGrid *Grid;
  TcxGridDBTableView *tvFilms;
  TcxGridDBColumn *tvFilmsCAPTION;
  TcxGridDBColumn *tvFilmsYEAR;
  TcxGridDBColumn *tvFilmsRUNTIME;
  TcxGridDBColumn *tvFilmsPHOTO;
  TcxGridDBColumn *tvFilmsTAGLINE;
  TcxGridDBColumn *tvFilmsPLOTOUTLINE;
  TcxGridLevel *lvFilms;
  TcxGridLevel *lvFilmsPersonsStaff;
  TMenuItem *miOptions;
  TMenuItem *miGrid;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall miGridClick(TObject *Sender);
private:  // User declarations
  void SetStandardMasterDetailStyle(void);
  void SetGridMasterDetailStyle(void);
public:   // User declarations
  __fastcall TMasterDetailTableDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMasterDetailTableDemoMainForm *MasterDetailTableDemoMainForm;
//---------------------------------------------------------------------------
#endif
