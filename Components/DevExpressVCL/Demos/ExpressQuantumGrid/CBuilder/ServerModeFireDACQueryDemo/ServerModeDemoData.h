//---------------------------------------------------------------------------

#ifndef ServerModeDemoDataH
#define ServerModeDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxServerModeFireDACDataSource.hpp"
#include "dxServerModeData.hpp"
#include <DB.hpp>
#include <DateUtils.hpp>
#include "ServerModeDemoConnection.h"
#if (__BORLANDC__ >= 0x670)
#include "FireDAC.Phys.MSSQL.hpp"
#include "FireDAC.Comp.UI.hpp"
#include "FireDAC.VCLUI.Wait.hpp"
#include "FireDAC.Comp.Client.hpp"
#include "FireDAC.Dapt.hpp"
#include "FireDAC.Phys.hpp"
#include "FireDAC.Stan.Async.hpp"
#include "FireDAC.Stan.Pool.hpp"
#include "FireDAC.Stan.Def.hpp"
#include "FireDAC.Phys.Intf.hpp"
#include "FireDAC.UI.Intf.hpp"
#include "FireDAC.Stan.Error.hpp"
#include "FireDAC.Stan.Option.hpp"
#include "FireDAC.Stan.Intf.hpp"
#else
#include "uADCompClient.hpp"
#include "uADCompGUIx.hpp"
#include "uADGUIxFormsWait.hpp"
#include "uADGUIxIntf.hpp"
#include "uADPhysIntf.hpp"
#include "uADPhysManager.hpp"
#include "uADStanAsync.hpp"
#include "uADStanDef.hpp"
#include "uADStanError.hpp"
#include "uADStanIntf.hpp"
#include "uADStanOption.hpp"
#include "uADStanPool.hpp"
#include "uADCompDataSet.hpp"
#include "uADDAptIntf.hpp"
#include "uADDAptManager.hpp"
#include "uADDatSManager.hpp"
#include "uADPhysMSSQL.hpp"
#include "uADPhysODBCBase.hpp"
#include "uADStanParam.hpp"
#endif
//---------------------------------------------------------------------------
class TServerModeDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
	TdxServerModeFireDACQueryDataSource *ServerModeQueryDataSource;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall DataModuleDestroy(TObject *Sender);
	void __fastcall ServerModeQueryDataSourceFatalError(TdxServerModeCustomDataSource *Sender, const String AMessage);
	void __fastcall ServerModeQueryDataSourceInconsistentCache(TdxServerModeCustomDataSource *Sender, const String AMessage,
          bool &ARecoverCache);
private:	// User declarations
    #if (__BORLANDC__ >= 0x670)
	TFDGUIxWaitCursor *ADGUIxWaitCursor1;
	TFDPhysMSSQLDriverLink *ADPhysMSSQLDriverLink1;
	TFDConnection *ADConnection;
	TFDQuery *ADQuery;
    #else
	TADGUIxWaitCursor *ADGUIxWaitCursor1;
	TADPhysMSSQLDriverLink *ADPhysMSSQLDriverLink1;
	TADConnection *ADConnection;
	TADQuery *ADQuery;
    #endif
	int DoGetRecordsCount(String ATableName);
	String GetCustomerInsertSQL();
	String GetOrderInsertSQL();
public:		// User declarations
	__fastcall TServerModeDemoDataDM(TComponent* Owner);
	void __fastcall AddRecords(int ACount, TdxProgressEvent AProgress);
	void Connect(String AHostName, String ADatabaseName, String AUserName, String APassword, Boolean OSAuthentication);
	void ExecSQL(String ASQL);
	int GetRecordsCount();
	void CreateDatabase();
	void CreateTable();
};
//---------------------------------------------------------------------------
String GetCaption();
String GetDatabaseName();
String GetOrdersTableName();
String GetCustomersTableName();
String GetDescription();
//---------------------------------------------------------------------------
extern PACKAGE TServerModeDemoDataDM *ServerModeDemoDataDM;
//---------------------------------------------------------------------------
#endif
