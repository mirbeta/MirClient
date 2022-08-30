//---------------------------------------------------------------------------

#ifndef ServerModeDemoDataH
#define ServerModeDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxServerModeDBXDataSource.hpp"
#include "dxServerModeData.hpp"
#include <DB.hpp>
#include <FMTBcd.hpp>
#include <SqlExpr.hpp>
#include <WideStrings.hpp>
#include <DateUtils.hpp>
#include "ServerModeDemoConnection.h"

#if (__CODEGEARC__ >= 0x0610) // C++Builder 2009 and later
  #include <DBXMSSQL.hpp>
#endif
//---------------------------------------------------------------------------
class TServerModeDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
	TSQLConnection *SQLConnection;
	TSQLQuery *SQLQuery;
	TdxServerModeDBXQueryDataSource *ServerModeQueryDataSource;
	void __fastcall ServerModeQueryDataSourceFatalError(TdxServerModeCustomDataSource *Sender, const String AMessage);
	void __fastcall ServerModeQueryDataSourceInconsistentCache(TdxServerModeCustomDataSource *Sender, const String AMessage,
          bool &ARecoverCache);
private:	// User declarations
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
