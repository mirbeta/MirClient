//---------------------------------------------------------------------------

#include <vcl.h>
#include "dstring.h"
#pragma hdrstop

#include "ServerModeDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxServerModeFireDACDataSource"
#pragma link "dxServerModeData"
#pragma resource "*.dfm"
TServerModeDemoDataDM *ServerModeDemoDataDM;
//---------------------------------------------------------------------------
String GetCaption(){
  return "ExpressQuantumGrid FireDAC Server Mode Demo";
}
//---------------------------------------------------------------------------
String GetDatabaseName(){
  return "ServerModeGridDemo";
}
//---------------------------------------------------------------------------
String GetTableName(){
  return "ServerModeGridTableDemo";
}
//---------------------------------------------------------------------------
String GetDescription(){
  return "This demo shows a grid control's capabilities when bound to a large amount of data in Server Mode via a FireDAC connection.";
}
//---------------------------------------------------------------------------
__fastcall TServerModeDemoDataDM::TServerModeDemoDataDM(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoDataDM::AddRecords(int ACount, TdxProgressEvent AProgress)
{
  int ASubCount = 10;
  String ASQL;
  ADConnection->StartTransaction();
  try
	{
	  for (int I=0;I<(ACount/ASubCount);I++)
	  {
		if ((I % 100) == 0)
		  AProgress(this, I*ASubCount*100.0/ACount);
		ASQL = "";
		for (int J=1;J<=ASubCount;J++)
		{
		  ASQL = ASQL + GetInsertSQL();
		}
		ADConnection->ExecSQL(ASQL);
	  }
	  ADConnection->Commit();
	}
  __except(EXCEPTION_EXECUTE_HANDLER)
  {
	ADConnection->Rollback();
  }
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::Connect(String AHostName, String ADatabaseName, String AUserName, String APassword, Boolean OSAuthentication)
{
  ADConnection->Connected = False;
  ADConnection->LoginPrompt = False;
  ADConnection->Params->Values["DriverID"] =  "MSSQL";
  if (OSAuthentication)
  {
	ADConnection->Params->Values["OSAuthent"] = "Yes";
  }
  else
  {
	ADConnection->Params->Values["OSAuthent"] = "No";
	ADConnection->Params->Values["User_Name"] = AUserName;
	ADConnection->Params->Values["Password"] = APassword;
  }
  ADConnection->Params->Values["Server"] = AHostName;
  ADConnection->Params->Values["Database"] = ADatabaseName;

  ADConnection->Connected = True; //# can get an exception on errors
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::CreateDatabase()
{
	String ASQL = "IF DB_ID(N\'%s\') IS NULL CREATE DATABASE \"%s\";";
	TVarRec args[2] = {GetDatabaseName(), GetDatabaseName()};
	ExecSQL(Format(ASQL, args, 2));
}
//---------------------------------------------------------------------------
bool TServerModeDemoDataDM::TableExists()
{
  String ASQL;
  ASQL = "IF DB_ID(N'" + GetDatabaseName() + "') IS NOT NULL AND OBJECT_ID(N'" + GetDatabaseName() +
	".dbo." + GetTableName() + "') IS NOT NULL" + sLineBreak +
	"  SELECT 1;" + sLineBreak +
	"ELSE" + sLineBreak +
	"  SELECT -1;";
  ADQuery->Active = False;
  ADQuery->Open(ASQL);
  if ((!ADQuery->Eof)&&(ADQuery->Fields->Fields[0]->AsInteger == 1))
	return true;
  else
	return false;
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::CreateTable()
{
  String ASQL;
  ASQL = "IF OBJECT_ID(N\'" + GetDatabaseName() + ".dbo." + GetTableName() + "\') IS NULL" + sLineBreak +
	"CREATE TABLE \"dbo\".\"" + GetTableName() + "\"(" +
	" \"OID\" int IDENTITY(1,1) NOT NULL, \"Subject\" nvarchar(100) NULL," +
	"\"From\" nvarchar(100) NULL," +
	" \"Sent\" datetime NULL," +
	" \"Size\" bigint NULL," +
	" \"HasAttachment\" bit NULL," +
	" \"Priority\" int NULL," +
	" CONSTRAINT \"PK_' + ATableName + '\" PRIMARY KEY CLUSTERED" +
	"(" +
	" \"OID\" ASC" +
	")WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON \"PRIMARY\"" +
	") ON \"PRIMARY\";";
  ExecSQL(ASQL);
  ExecSQL("CREATE NONCLUSTERED INDEX iSubject_ServerModeGridTableDemo ON \"ServerModeGridTableDemo\" (\"Subject\");");
  ExecSQL("CREATE NONCLUSTERED INDEX iFrom_ServerModeGridTableDemo ON \"ServerModeGridTableDemo\" (\"From\");");
  ExecSQL("CREATE NONCLUSTERED INDEX iSent_ServerModeGridTableDemo ON \"ServerModeGridTableDemo\" (\"Sent\");");
  ExecSQL("CREATE NONCLUSTERED INDEX iSize_ServerModeGridTableDemo ON \"ServerModeGridTableDemo\" (\"Size\");");
  ExecSQL("CREATE NONCLUSTERED INDEX iHasAttachment_ServerModeGridTableDemo ON \"ServerModeGridTableDemo\" (\"HasAttachment\");");
  ExecSQL("CREATE NONCLUSTERED INDEX iPriority_ServerModeGridTableDemo ON \"ServerModeGridTableDemo\" (\"Priority\");");
}
//---------------------------------------------------------------------------

void TServerModeDemoDataDM::ExecSQL(String ASQL)
{
  ADQuery->Active = false;
  ADQuery->SQL->Text = ASQL;
  ADQuery->ExecSQL();
}
//---------------------------------------------------------------------------
String TServerModeDemoDataDM::GetInsertSQL(){
  String Users[] = { //17
	"Peter Dolan",
	"Ryan Fischer",
	"Richard Fisher",
	"Tom Hamlett",
	"Mark Hamilton",
	"Steve Lee",
	"Jimmy Lewis",
	"Jeffrey W McClain",
	"Andrew Miller",
	"Dave Murrel",
	"Bert Parkins",
	"Mike Roller",
	"Ray Shipman",
	"Paul Bailey",
	"Brad Barnes",
	"Carl Lucas",
	"Jerry Campbell"
  };
  String Subjects[] = { //21
	"Integrating Developer Express MasterView control into an Accounting System.",
    "Web Edition: Data Entry Page. There is an issue with date validation.",
	"Payables Due Calculator is ready for testing.",
	"Web Edition: Search Page is ready for testing.",
	"Main Menu: Duplicate Items. Somebody has to review all menu items in the system.",
	"Receivables Calculator. Where can I find the complete specs?",
	"Ledger: Inconsistency. Please fix it.",
	"Receivables Printing module is ready for testing.",
	"Screen Redraw. Somebody has to look at it.",
	"Email System. What library are we going to use?",
	"Cannot add new vendor. This module doesn""""t work!",
	"History. Will we track sales history in our system?",
	"Main Menu: Add a File menu. File menu item is missing.",
	"Currency Mask. The current currency mask in completely unusable.",
	"Drag & Drop operations are not available in the scheduler module.",
	"Data Import. What database types will we support?",
	"Reports. The list of incomplete reports.",
	"Data Archiving. We still don""""t have this features in our application.",
	"Email Attachments. Is it possible to add multiple attachments? I haven""""t found a way to do this.",
	"Check Register. We are using different paths for different modules.",
	"Data Export. Our customers asked us for export to Microsoft Excel"
  };
  String S = FormatDateTime("yyyymmdd hh:mm:ss", IncSecond(Now(), -Random(315360000)));
  return "INSERT INTO \"" + GetTableName() + "\" (\"Subject\", \"From\", \"Sent\", \"Size\", \"HasAttachment\", \"Priority\")" +
	"VALUES (\'" + Subjects[Random(21)] + "\',\'" + Users[Random(17)] + "\',\'" + S + "\'," +
	IntToStr(Random(100000)) + "," + IntToStr(Random(2)) + "," + IntToStr(Random(3)) + ");";
}
//---------------------------------------------------------------------------
int TServerModeDemoDataDM::GetRecordsCount()
{
  ADQuery->Active = false;
  String ASQL = "IF DB_ID(N\'%s\') IS NOT NULL AND OBJECT_ID(N\'%0:s.dbo.%1:s\') IS NOT NULL SELECT COUNT(*) FROM %0:s.dbo.%1:s; ELSE SELECT 0;";
  TVarRec args[2] = {GetDatabaseName(), GetTableName()};
  ADQuery->Open(Format(ASQL, args, 2));
  if (!ADQuery->Eof)
	return ADQuery->Fields->Fields[0]->AsInteger;
  else
	return 0;
}

void __fastcall TServerModeDemoDataDM::DataModuleCreate(TObject *Sender)
{
#if (__BORLANDC__ >= 0x670)
	ADConnection = new TFDConnection(NULL);
	ADQuery = new TFDQuery(NULL);
#else
	ADConnection = new TADConnection(NULL);
	ADQuery = new TADQuery(NULL);
#endif
  ADConnection->LoginPrompt = false;
  ADQuery->Connection = ADConnection;
  ADQuery->ResourceOptions->AssignedValues << rvDirectExecute;
  ADQuery->ResourceOptions->DirectExecute = true;
  ServerModeDataSource->SQLAdapterClassName = "TdxServerModeMSSQLAdapter";
  ServerModeDataSource->Connection = ADConnection;
}
//---------------------------------------------------------------------------

void __fastcall TServerModeDemoDataDM::DataModuleDestroy(TObject *Sender)
{
  delete(ADQuery);
  delete(ADConnection);
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoDataDM::ServerModeDataSourceFatalError(TdxServerModeCustomDataSource *Sender, const String AMessage)
{
  String S = "The following error occurred when obtaining data: \"" + AMessage + "\".\n";
  S += "Do you want to reload the data?";
  if (MessageDlg(S, mtError, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
	Sender->Open();
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoDataDM::ServerModeDataSourceInconsistentCache(TdxServerModeCustomDataSource *Sender, const String AMessage, bool &ARecoverCache)
{
  ARecoverCache = MessageDlg("The cache state is inconsistent. Do you want to recover it?", mtError, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes;
  if (!ARecoverCache)
	Sender->Close();
}
//---------------------------------------------------------------------------

