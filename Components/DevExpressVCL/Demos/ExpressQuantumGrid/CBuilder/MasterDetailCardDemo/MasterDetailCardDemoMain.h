//---------------------------------------------------------------------------

#ifndef MasterDetailCardDemoMainH
#define MasterDetailCardDemoMainH
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
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxDBEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxLookupEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxTextEdit.hpp"
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include "cxDBLookupEdit.hpp"
#include "cxNavigator.hpp"
#include "cxDBNavigator.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TMasterDetailCardDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
	TcxLabel *lblMaster;
	TcxLabel *lblStyle;
	TBevel *Bevel1;
	TcxGrid *Grid;
	TcxGridDBTableView *tvFilms;
	TcxGridDBColumn *colFilmsCaption;
	TcxGridDBColumn *colFilmsYear;
	TcxGridDBColumn *colFilmsRuntime;
	TcxGridDBColumn *colFilmsPhoto;
	TcxGridDBColumn *colFilmsTagline;
	TcxGridDBColumn *colFilmsPlotOutline;
	TcxGridDBCardView *cvFilmsPersons;
	TcxGridDBCardViewRow *cvFilmsPersonsName;
	TcxGridDBCardViewRow *cvFilmsPersonsPersonLineID;
	TcxGridDBCardViewRow *cvFilmsPersonsFIRSTNAME;
	TcxGridDBCardViewRow *cvFilmsPersonsSECONDNAME;
	TcxGridDBCardViewRow *cvFilmsPersonsNICKNAME;
	TcxGridDBCardViewRow *cvFilmsPersonsDATEOFBIRTH;
	TcxGridDBCardViewRow *cvFilmsPersonsLOCATIONOFBIRTH;
	TcxGridDBCardViewRow *cvFilmsPersonsBIRTHNAME;
	TcxGridDBCardViewRow *cvFilmsPersonsBIOGRAPHY;
	TcxGridDBCardViewRow *cvFilmsPersonsGender;
	TcxGridDBCardViewRow *cvFilmsPersonsHOMEPAGE;
	TcxGridLevel *lvFilms;
	TcxGridLevel *lvFilmsPersons;
	TPanel *pnlDetail;
	TcxLabel *lblDetail;
	TPanel *Panel1;
	TcxLabel *Label1;
	TcxLabel *Label2;
	TcxLabel *Label8;
	TcxLabel *Label9;
	TcxLabel *Label7;
	TcxLabel *Label6;
	TcxLabel *Label4;
	TcxLabel *Label3;
	TcxLabel *Label5;
	TcxDBLookupComboBox *cbOccupation;
	TcxDBTextEdit *edFirstName;
	TcxDBTextEdit *edSecondName;
	TcxDBTextEdit *edNickName;
	TcxDBHyperLinkEdit *edHomePage;
	TcxDBNavigator *DBNavigator1;
	TcxDBTextEdit *edBirthName;
	TcxDBTextEdit *edLocationOfBirth;
	TcxDBDateEdit *deDateOfBirth;
	TcxDBMemo *meBiography;
	TcxDBCheckBox *chbMale;
	TMenuItem *miOptions;
	TMenuItem *miGrid;
	TcxEditStyleController *cxEditStyleController1;
	void __fastcall miGridClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:  // User declarations
	void SetStandardMasterDetailStyle(void);
  	void SetGridMasterDetailStyle(void);
public:   // User declarations
  __fastcall TMasterDetailCardDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMasterDetailCardDemoMainForm *MasterDetailCardDemoMainForm;
//---------------------------------------------------------------------------
#endif
