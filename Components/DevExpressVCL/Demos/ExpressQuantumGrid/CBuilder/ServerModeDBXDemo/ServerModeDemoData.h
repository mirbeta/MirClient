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
	TdxServerModeDBXDataSource *ServerModeDataSource;
	void __fastcall ServerModeDataSourceFatalError(TdxServerModeCustomDataSource *Sender, const String AMessage);
	void __fastcall ServerModeDataSourceInconsistentCache(TdxServerModeCustomDataSource *Sender, const String AMessage,
          bool &ARecoverCache);
private:	// User declarations
	String GetInsertSQL();
public:		// User declarations
	__fastcall TServerModeDemoDataDM(TComponent* Owner);
	void __fastcall AddRecords(int ACount, TdxProgressEvent AProgress);
	void Connect(String AHostName, String ADatabaseName, String AUserName, String APassword, Boolean OSAuthentication);
	void ExecSQL(String ASQL);
	int GetRecordsCount();
	void CreateDatabase();
	bool TableExists();
	void CreateTable();
};
//---------------------------------------------------------------------------
String GetCaption();
String GetDatabaseName();
String GetTableName();
String GetDescription();
//---------------------------------------------------------------------------
extern PACKAGE TServerModeDemoDataDM *ServerModeDemoDataDM;
//---------------------------------------------------------------------------
#endif
