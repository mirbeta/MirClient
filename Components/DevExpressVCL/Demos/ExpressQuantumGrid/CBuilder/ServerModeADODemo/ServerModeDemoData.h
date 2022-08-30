//---------------------------------------------------------------------------

#ifndef ServerModeDemoDataH
#define ServerModeDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxServerModeADODataSource.hpp"
#include "dxServerModeData.hpp"
#include <ADODB.hpp>
#include <DB.hpp>
#include <DateUtils.hpp>
#include "ServerModeDemoConnection.h"
//---------------------------------------------------------------------------
class TServerModeDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
	TADOConnection *ADOConnection;
	TADOQuery *ADOQuery;
	TdxServerModeADODataSource *ServerModeDataSource;
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
