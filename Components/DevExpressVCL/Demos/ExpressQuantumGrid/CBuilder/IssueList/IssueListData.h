//---------------------------------------------------------------------------

#ifndef IssueListDataH
#define IssueListDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxDBEditRepository.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <ImgList.hpp>
#include <Db.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TdmMain : public TDataModule
{
__published:	// IDE-managed Components
        TDataSource *dsUsers;
        TDataSource *dsItems;
        TDataSource *dsProjects;
        TImageList *imStat;
        TImageList *ilMain;
        TcxEditRepository *edrepMain;
        TcxEditRepositoryLookupComboBoxItem *edrepUserLookup;
        TcxEditRepositoryLookupComboBoxItem *edrepUserFullName;
        TcxEditRepositoryLookupComboBoxItem *edrepDepartmentName;
        TcxEditRepositoryLookupComboBoxItem *edrepProjectName;
        TcxEditRepositoryImageComboBoxItem *edrepItemStatus;
        TcxEditRepositoryImageComboBoxItem *edrepItemType;
        TcxEditRepositoryImageComboBoxItem *edrepItemPriority;
        TcxStyleRepository *strepMain;
        TcxStyle *stFixed;
        TcxStyle *stRejected;
        TcxStyle *stNew;
        TcxStyle *stPostponed;
        TcxStyle *stPreview;
        TcxStyle *stLightYellow;
        TcxStyle *stSelected;
        TcxStyle *stLightBlue;
        TcxStyle *stGold;
        TcxStyle *stBlue;
        TcxGridTableViewStyleSheet *ssTableStyles;
        TcxEditStyleController *edstcMain;
	TClientDataSet *cdsUsers;
	TAutoIncField *cdsUsersID;
	TStringField *cdsUsersUserName;
	TStringField *cdsUsersFNAME;
	TStringField *cdsUsersMNAME;
	TStringField *cdsUsersLNAME;
	TStringField *cdsUsersCOUNTRY;
	TStringField *cdsUsersPOSTALCODE;
	TStringField *cdsUsersCITY;
	TStringField *cdsUsersADDRESS;
	TStringField *cdsUsersPHONE;
	TStringField *cdsUsersFAX;
	TStringField *cdsUsersEMAIL;
	TStringField *cdsUsersHOMEPAGE;
	TIntegerField *cdsUsersDEPARTMENTID;
	TStringField *cdsUsersDepartment;
	TStringField *cdsUsersFullName;
	TClientDataSet *cdsDepartments;
	TAutoIncField *cdsDepartmentsID;
	TStringField *cdsDepartmentsNAME;
	TClientDataSet *cdsItems;
	TAutoIncField *cdsItemsID;
	TStringField *cdsItemsNAME;
	TBooleanField *cdsItemsTYPE;
	TIntegerField *cdsItemsPROJECTID;
	TSmallintField *cdsItemsPRIORITY;
	TSmallintField *cdsItemsSTATUS;
	TIntegerField *cdsItemsCREATORID;
	TDateTimeField *cdsItemsCREATEDDATE;
	TIntegerField *cdsItemsOWNERID;
	TDateTimeField *cdsItemsLASTMODIFIEDDATE;
	TDateTimeField *cdsItemsFIXEDDATE;
	TMemoField *cdsItemsDESCRIPTION;
	TMemoField *cdsItemsRESOLUTION;
	TClientDataSet *cdsProjects;
	TAutoIncField *cdsProjectsID;
	TStringField *cdsProjectsNAME;
	TIntegerField *cdsProjectsMANAGERID;
	TClientDataSet *cdsTeam;
	TAutoIncField *cdsTeamID;
	TIntegerField *cdsTeamPROJECTID;
	TIntegerField *cdsTeamUSERID;
	TStringField *cdsTeamFUNCTION;
	TClientDataSet *cdsScheduler;
	TAutoIncField *cdsSchedulerID;
	TIntegerField *cdsSchedulerPROJECTID;
	TIntegerField *cdsSchedulerUSERID;
	TSmallintField *cdsSchedulerSUNDAY;
	TSmallintField *cdsSchedulerMONDAY;
	TSmallintField *cdsSchedulerTUESDAY;
	TSmallintField *cdsSchedulerWEDNESDAY;
	TSmallintField *cdsSchedulerTHURSDAY;
	TSmallintField *cdsSchedulerFRIDAY;
	TSmallintField *cdsSchedulerSATURDAY;
	TStringField *cdsSchedulerUserName;
	TStringField *cdsSchedulerFirstName;
	TStringField *cdsSchedulerMiddleName;
	TStringField *cdsSchedulerLastName;
	TFloatField *cdsSchedulerRowSum;
	TFloatField *cdsSchedulerRowAvg;
        void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall cdsUsersCalcFields(TDataSet *DataSet);
	void __fastcall cdsSchedulerCalcFields(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
        __fastcall TdmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmMain *dmMain;
//---------------------------------------------------------------------------
#endif
