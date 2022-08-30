//---------------------------------------------------------------------------

#include <vcl.h>
#include "dstring.h"
#pragma hdrstop

#include "ServerModeDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxServerModeADODataSource"
#pragma link "dxServerModeData"
#pragma resource "*.dfm"
TServerModeDemoDataDM *ServerModeDemoDataDM;
//---------------------------------------------------------------------------
String GetCaption(){
  return "ExpressQuantumGrid ADO Server Mode Query Demo";
}
//---------------------------------------------------------------------------
String GetDatabaseName(){
  return "ServerModeGridDemo";
}
//---------------------------------------------------------------------------
String GetOrdersTableName(){
  return "ServerModeGridOrdersDemo";
}
//---------------------------------------------------------------------------
String GetCustomersTableName(){
  return "ServerModeGridCustomersDemo";
}
//---------------------------------------------------------------------------
String GetDescription(){
  return "This demo shows a grid control's capabilities when bound to a large amount of data in Server Mode via an ADO connection. The data is retrieved using an ADO query.";
}
//---------------------------------------------------------------------------
__fastcall TServerModeDemoDataDM::TServerModeDemoDataDM(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoDataDM::AddRecords(int ACount, TdxProgressEvent AProgress)
{
  String ASQL;
  int AMaxCustomerCount = 100;
  int ASubCount = 10;
  int ACustomerCount = DoGetRecordsCount(GetCustomersTableName());
  if (ACustomerCount < AMaxCustomerCount)
  {
	  ACustomerCount = AMaxCustomerCount - ACustomerCount;
	  ADOConnection->BeginTrans();
	  try
		{
		  for (int I=0;I<(ACustomerCount/ASubCount);I++)
		  {
			ASQL = "";
			for (int J=1;J<=ASubCount;J++)
			{
			  ASQL = ASQL + GetCustomerInsertSQL();
			}
			ADOConnection->Execute(ASQL);
		  }
		  ADOConnection->CommitTrans();
		  }
	  __except(EXCEPTION_EXECUTE_HANDLER)
	  {
		ADOConnection->RollbackTrans();
	  }
  }
  ADOConnection->BeginTrans();
  try
	{
	  for (int I=0;I<(ACount/ASubCount);I++)
	  {
		if ((I % 100) == 0)
		  AProgress(this, I*ASubCount*100.0/ACount);
		ASQL = "";
		for (int J=1;J<=ASubCount;J++)
		{
		  ASQL = ASQL + GetOrderInsertSQL();
		}
		ADOConnection->Execute(ASQL);
	  }
	  ADOConnection->CommitTrans();
	  }
  __except(EXCEPTION_EXECUTE_HANDLER)
  {
	ADOConnection->RollbackTrans();
  }
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::Connect(String AHostName, String ADatabaseName, String AUserName, String APassword, Boolean OSAuthentication)
{
  String AConnectionString;
  ADOConnection->Connected = false;
  ADOConnection->Provider = "SQLOLEDB.1";
  ADOConnection->LoginPrompt = false;
  String S = "Provider=SQLOLEDB.1;Data Source=%s;Initial Catalog=%s";
  TVarRec args[2] = {AHostName, ADatabaseName};
  AConnectionString = Format(S, args, 2);
  if (OSAuthentication)
	ADOConnection->ConnectionString = AConnectionString + ";Integrated Security=SSPI;Persist Security Info=False";
  else
  {
	TVarRec args2[2] = {AUserName, APassword};
	S = ";User ID=%s;Password=%s;Persist Security Info=True";
	ADOConnection->ConnectionString = AConnectionString +
	  Format(S, args2, 2);
  }
  ADOConnection->Connected = true;
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::CreateDatabase()
{
	String ASQL = "IF DB_ID(N\'%s\') IS NULL CREATE DATABASE \"%s\";";
	TVarRec args[2] = {GetDatabaseName(), GetDatabaseName()};
	ExecSQL(Format(ASQL, args, 2));
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::CreateTable()
{
  String ASQL;

  ASQL = "IF OBJECT_ID(N\'" + GetDatabaseName() + ".dbo." + GetCustomersTableName() + "\') IS NULL" + sLineBreak +
	"CREATE TABLE \"dbo\".\"" + GetCustomersTableName() + "\"(" +
	" \"OID\" int IDENTITY(1,1) NOT NULL," +
	" \"FirstName\" nvarchar(10) NOT NULL," +
	" \"LastName\" nvarchar(10) NOT NULL," +
	" \"Company\" nvarchar(30) NOT NULL," +
	" \"Prefix\" nvarchar(5) NOT NULL," +
	" \"Title\" nvarchar(5) NOT NULL," +
	" \"Address\" nvarchar(50) NOT NULL," +
	" \"City\" nvarchar(15) NOT NULL, " +
	" \"State\" nvarchar(2) NOT NULL," +
	" \"ZipCode\" nvarchar(10) NOT NULL," +
	" \"Source\" nvarchar(10) NOT NULL," +
	" \"Customer\" bit NULL," +
	" \"HomePhone\" nvarchar(15) NOT NULL," +
	" \"FaxPhone\" nvarchar(15) NOT NULL," +
	" \"Description\" text NOT NULL," +
	" \"Email\" nvarchar(30) NOT NULL" +
	" CONSTRAINT \"PK_" + GetCustomersTableName() + "\" PRIMARY KEY CLUSTERED" +
	"(" +
	" \"OID\" ASC" +
	")WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON \"PRIMARY\"" +
	") ON \"PRIMARY\";";
  ExecSQL(ASQL);

  ASQL = "IF OBJECT_ID(N\'" + GetDatabaseName() + ".dbo." + GetOrdersTableName() + "\') IS NULL" + sLineBreak +
	"CREATE TABLE \"dbo\".\"" + GetOrdersTableName() + "\"(" +
	" \"OID\" int IDENTITY(1,1) NOT NULL," +
	" \"CustomerID\" bigint NOT NULL," +
	" \"OrderDate\" datetime NULL," +
	" \"Trademark\" nvarchar(20) NOT NULL," +
	" \"Model\" nvarchar(30) NOT NULL," +
	" \"HP\" bigint NOT NULL," +
	" \"Cyl\" bigint NOT NULL," +
	" \"TransmissSpeedCount\" bigint NOT NULL," +
	" \"TransmissAutomatic\" bit NULL," +
	" \"Category\" nvarchar(10) NOT NULL," +
	" \"Price\" bigint NOT NULL," +
	" CONSTRAINT \"PK_" + GetOrdersTableName() + "\" PRIMARY KEY CLUSTERED" +
	"(" +
	" \"OID\" ASC" +
	")WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON \"PRIMARY\"" +
	") ON \"PRIMARY\";";
  ExecSQL(ASQL);
}
//---------------------------------------------------------------------------
int TServerModeDemoDataDM::DoGetRecordsCount(String ATableName)
{
  ADOQuery->Active = false;
  String ASQL = "IF DB_ID(N\'%s\') IS NOT NULL AND OBJECT_ID(N\'%0:s.dbo.%1:s\') IS NOT NULL SELECT COUNT(*) FROM %0:s.dbo.%1:s; ELSE SELECT 0;";
  TVarRec args[2] = {GetDatabaseName(), ATableName};
  ADOQuery->SQL->Text = Format(ASQL, args, 2);
  ADOQuery->Open();
  if (!ADOQuery->Eof)
	return ADOQuery->Fields->Fields[0]->AsInteger;
  else
	return 0;
}
//---------------------------------------------------------------------------
void TServerModeDemoDataDM::ExecSQL(String ASQL)
{
  ADOQuery->Active = false;
  ADOQuery->SQL->Text = ASQL;
  ADOQuery->ExecSQL();
}
//---------------------------------------------------------------------------
String TServerModeDemoDataDM::GetCustomerInsertSQL()
{
  String AFirstName[] = {//19
	"Jane",
	"Sam",
	"Karen",
	"Bobbie",
	"Ricardo",
	"Frank",
	"Christa",
	"Jimmie",
	"Alfred",
	"James",
	"Robert",
	"June",
	"Mildred",
	"Henry",
	"Michael",
	"Scott",
	"Mickey",
	"Roger",
	"Leticia"
  };
  String ALastName[] = {//19
	"Doe",
	"Hill",
	"Holmes",
	"Valentine",
	"Menendez",
	"Frankson",
	"Christie",
	"Jones",
	"Newman",
	"Johnson",
	"James",
	"Alessandro",
	"Johansson",
	"McAllister",
	"Jeffers",
	"Mathewson",
	"Alcorn",
	"Michelson",
	"Ford"
  };
  String ACompany[] = {//19
	"Doe Enterprises",
	"Hill Corporation",
	"Holmes World",
	"Valentine Hearts",
	"Menedez Development",
	"Frankson Media",
	"Christies House of Design",
	"Jones & Assoc",
	"Newman Systems",
	"Development House",
	"James Systems",
	"Alessandro & Associates",
	"Mildreds World",
	"McAllister Systems",
	"Jeffers Clinic",
	"Mathewson Design",
	"Mickeys World of Fun","Michelson Systems",
	"Ford Consulting"
  };
  String APrefix[] = {//19
	"Ms.",
	"Mr.",
	"Ms.",
	"Mr.",
	"Mr.",
	"Mr.",
	"Ms.",
	"Mrs.",
	"Mr.",
	"Mr.",
	"Mr.",
	"Mrs.",
	"Ms.",
	"Mr.",
	"Mr.",
	"Mr.",
	"Mr.",
	"Mr.",
	"Mrs."
  };
  String ATitle[] = {//19
	"",
	"",
	"",
	"Dr.",
	"",
	"",
	"PhD",
	"",
	"PhD",
	"PhD",
	"MS",
	"PhD",
	"",
	"MS",
	"PhD",
	"MA",
	"",
	"PhD",
	"PhD"
  };
  String AAddress[] = {//19
	"123 Home Lane",
	"45 Hill St.",
	"9333 Holmes Dr.",
	"933 Heart St. Suite 1",
	"939 Center Street",
	"121 Media Center Drive",
	"349 Graphic Design Lane",
	"990 King Lane",
	"900 Newman Center",
	"93900 Carter Lane",
	"390-1 Fourth St.",
	"90283 Los Angeles Ave.",
	"390290 Mildred Lane",
	"029-938 Excelsior Way",
	"233 First St.",
	"111 McHenry St.",
	"436 1st Ave.",
	"3920 Michelson Dr.",
	"2900 Ford Drive"
  };
  String ACity[] =  {//19
	"Atlanta",
	"Hillsville",
	"Johnsonville",
	"Chicago",
	"Atlanta",
	"New York",
	"New York",
	"Kingsville",
	"Newman",
	"Cartersville",
	"New York",
	"Los Angeles",
	"Cleveland",
	"San Francisco",
	"Los Angeles",
	"New York",
	"Cleveland",
	"Bridgeford",
	"Lansing"
  };
  String AState[] = {//19
	"CA",
	"VA",
	"NY",
	"IL",
	"GA",
	"NY",
	"CA",
	"CA",
	"OK",
	"GA",
	"NY",
	"CA",
	"OH",
	"CA",
	"CA",
	"NY",
	"OH",
	"CT",
	"MI"
  };
  String AZipCode[] = {//19
	"55555",
	"44444",
	"12121",
	"89123",
	"45012",
	"12121",
	"12211",
	"93939",
	"9900-",
	"909309",
	"12121",
	"93090",
	"37288-2323",
	"98454",
	"92939",
	"11111-2222",
	"37288",
	"93200",
	"23920"
  };
  String ASource[] = {//19
	"",
	"NY Times",
	"Bob",
	"Jennie",
	"",
	"NY Times",
	"NY Times",
	"",
	"LA Times",
	"",
	"NY Times",
	"LA Times",
	"Referral",
	"Referral",
	"LA Times",
	"NY Times",
	"Referral",
	"",
	"Referral"
  };
  String AHomePhone[] = {//19
	"(233)323-33-23",
	"(222)222-22-22",
	"(333)333-33-33",
	"(898)745-15-11",
	"(151)615-16-11",
	"(339)339-39-39",
	"(939)399-99-99",
	"(993)939-93-99",
	"(930)930-30-93",
	"(309)209-30-20",
	"(029)020-90-90",
	"(902)092-09-30",
	"(090)983-02-48",
	"(923)022-08-34",
	"(093)008-30-23",
	"(209)093-84-98",
	"(000)002-32-32",
	"(203)290-89-02",
	"(228)320-83-20"
  };
  String AFaxPhone[] = {//19
	"(444)444-44-44",
	"(222)222-22-22",
	"(212)556-56-55",
	"(321)516-51-51",
	"(656)161-65-16",
	"(393)090-20-99",
	"(909)098-00-98",
	"(930)930-98-09",
	"(920)983-02-02",
	"(094)302-89-08",
	"(090)909-00-90",
	"(940)104-80-93",
	"(190)890-02-83",
	"(084)029-80-28",
	"(080)098-90-08",
	"(098)900-98-90",
	"(098)902-98-34",
	"(098)900-83-04",
	"(098)908-00-80"
  };
  String ADescription[] = {//19
	"This is a description for Jane Doe.\nNotice the Auto Preview Feature.",
	"This is a description for Sam Hill.\nNotice the Auto Preview Feature.",
	"This is a description for Karen Holmes.\nNotice the Auto Preview Feature.",
	"",
	"This is a description for Ricardo Menendez.\nNotice the Auto Preview Feature.",
	"This is a description for Frank Frankson.\nNotice the Auto Preview Feature.",
	"This is a description for Christa Christie.\nNotice the Auto Preview Feature.",
	"This is a description for Jimmie Jones.\nNotice the Auto Preview Feature.",
	"This is a description for Alfred Newman.\nNotice the Auto Preview Feature.",
	"This is a description for James Johnson.\nNotice the Auto Preview Feature.",
	"This is a description for Robert James.\nNotice the Auto Preview Feature.",
	"",
	"This is a description for Mildred Johansson.\nNotice the Auto Preview Feature.",
	"",
	"This is a description for Michael Jeffers.\nNotice the Auto Preview Feature.",
	"This is a description for Scott Mathewson.\nNotice the Auto Preview Feature.",
	"This is a description for Mickey Alcorn.\nNotice the Auto Preview Feature.",
	"This is a description for Mickey Alcorn.\nNotice the Auto Preview Feature.",
	""
  };
  String AEmail[] = {//19
	"doej@doeent.com",
	"hills@hillcorp.com",
	"holmesk@holmesw.com",
	"valentineb@valetntineh.com",
	"menendezr@menedezdev.com",
	"franksonf@frankfmedia.com",
	"christiec@christiesdesign.com",
	"jonesj@jonesjim.com",
	"newmanalf@newmansyst.com",
	"johnsonj@jdevhouse.com",
	"jamesr@jrsengin.com",
	"alessandroj@alessandroassoc",
	"johanssonm@mildrworld.com",
	"mcallisterh@mcallistersyst.com",
	"jeffersm@jeffersclinic.com",
	"mathewsons@mathewstondsgn.com",
	"alcornm@mikeysworld.com",
	"michelsonr@michelsonsyst.com",
	"fordl@fordconsult.com"
  };
  int ACustomerIndex = Random(19);
  int AAddressIndex = Random(19);
  String AIsCustomer;
  int i = Random(3);
  if (i == 0) {
	AIsCustomer = "0";
  }else  if (i == 1) {
	AIsCustomer = "1";
  }else{
	AIsCustomer = "NULL";
  }
  String ASQL = "INSERT INTO \"%s\" (\"FirstName\", \"LastName\", \"Company\", \"Prefix\", \"Title\", \"Address\", \"City\", \"State\", \"ZipCode\", \"Source\", \"Customer\", \"HomePhone\", \"FaxPhone\", \"Description\", \"Email\") VALUES ";
  ASQL = ASQL + "('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', %s, '%s', '%s', '%s', '%s')";
  TVarRec args[16] = {
	GetCustomersTableName(),
	AFirstName[ACustomerIndex],
	ALastName[ACustomerIndex],
	ACompany[ACustomerIndex],
	APrefix[ACustomerIndex],
	ATitle[ACustomerIndex],
	AAddress[AAddressIndex],
	ACity[AAddressIndex],
	AState[AAddressIndex],
	AZipCode[AAddressIndex],
	ASource[AAddressIndex],
	AIsCustomer,
	AHomePhone[Random(19)],
	AFaxPhone[Random(19)],
	ADescription[AAddressIndex],
	AEmail[AAddressIndex]
  };
  return Format(ASQL, args, 16);
}
//---------------------------------------------------------------------------
String TServerModeDemoDataDM::GetOrderInsertSQL()
{
  String ATrademark[24] = {
	"Mercedes-Benz",
	"Mercedes-Benz",
	"Mercedes-Benz",
	"BMW",
	"Rolls-Royce",
	"Jaguar",
	"Cadillac",
	"Cadillac",
	"Lexus",
	"Lexus",
	"Ford",
	"Dodge",
	"GMC",
	"Nissan","Toyota",
	"Infiniti",
	"Infiniti",
	"Jaguar",
	"Audi",
	"Audi",
	"BMW",
	"BMW",
	"Acura",
	"Acura"
  };
  String AModel[24] = {
	"SL500 Roadster",
	"CLK55 AMG Cabriolet",
	"C230 Kompressor Sport Coupe",
	"530i",
	"Corniche",
	"S-Type 3.0",
	"Seville",
	"DeVille",
	"LS430",
	"GS 430",
	"Ranger FX-4",
	"Ram 1500",
	"Siera Quadrasteer",
	"Crew Cab SE",
	"Tacoma S-Runner",
	"Q45",
	"G35 Sport Coupe Leather 6MT",
	"XK8 Coupe",
	"A6 3.0",
	"TT Roadster",
	"760i Sedan",
	"Z4 3.0 Roadster",
	"TSX",
	"NSX 3.2"
  };
  String AHP[24] = {
	"302",
	"342",
	"189",
	"225",
	"325",
	"235",
	"275",
	"275",
	"290",
	"300",
	"135",
	"215",
	"200",
	"143",
	"190",
	"340",
	"280",
	"294",
	"220",
	"180",
	"438",
	"225",
	"200",
	"290"
  };
  String ACyl[24] = {
	"8",
	"8",
	"4",
	"6",
	"8",
	"6",
	"8",
	"8",
	"8",
	"8",
	"4",
	"6",
	"6",
	"4",
	"6",
	"8",
	"6",
	"8",
	"6",
	"4",
	"12",
	"6",
	"4",
	"6"
  };
  String ATransmissSpeedCount[24] = {
	"5",
	"5",
	"5",
	"6",
	"4",
	"5",
	"4",
	"4",
	"5",
	"5",
	"5",
	"4",
	"4",
	"4",
	"5",
	"5",
	"6",
	"6",
	"5",
	"6",
	"6",
	"6",
	"6",
	"6"
  };
  String ATransmissAutomatic[24] = {
	"1",
	"1",
	"1",
	"0",
	"1",
	"0",
	"1",
	"1",
	"1",
	"1",
	"1",
	"1",
	"1",
	"1",
	"0",
	"1",
	"NULL",
	"1",
	"1",
	"1",
	"1",
	"NULL",
	"0",
	"NULL"
  };
  String ACategory[24] = {
	"SPORTS",
	"SPORTS",
	"SPORTS",
	"SALOON",
	"SALOON",
	"SALOON",
	"SALOON",
	"SALOON",
	"SALOON",
	"SALOON",
	"TRUCK",
	"TRUCK",
	"TRUCK",
	"TRUCK",
	"TRUCK",
	"SALOON",
	"SPORTS",
	"SPORTS",
	"SALOON",
	"SPORTS",
	"SALOON",
	"SPORTS",
	"SPORTS",
	"SPORTS"
  };
  String APrice[24] = {
	"83800",
	"79645",
	"25600",
	"39450",
	"370485",
	"44320",
	"49600",
	"47780",
	"54900",
	"41242",
	"12565",
	"17315",
	"17748",
	"12800",
	"20000",
	"62300",
	"34000",
	"73000",
	"38000",
	"45000",
	"120000",
	"45000",
	"28500",
	"95000"
  };
  int AMaxCustomerCount = 100;
  int I = Random(24);
  String ASQL = "INSERT INTO \"%s\" (\"CustomerID\", \"OrderDate\", \"Trademark\", \"Model\", \"HP\", \"Cyl\", \"TransmissSpeedCount\", \"TransmissAutomatic\", \"Category\", \"Price\") VALUES ('%d', '%s', '%s', '%s', '%s', '%s', '%s', %s, '%s', '%s');";
  TVarRec args[11] = {
	GetOrdersTableName(),
	Random(AMaxCustomerCount) + 1,
	FormatDateTime("yyyymmdd hh:mm:ss", IncSecond(Now(), -Random(315360000))),
	ATrademark[I],
	AModel[I],
	AHP[I],
	ACyl[I],
	ATransmissSpeedCount[I],
	ATransmissAutomatic[I],
	ACategory[I],
	APrice[I]
  };
  return Format(ASQL, args, 11);
}
//---------------------------------------------------------------------------
int TServerModeDemoDataDM::GetRecordsCount()
{
  if (DoGetRecordsCount(GetCustomersTableName()) > 0)
	return DoGetRecordsCount(GetOrdersTableName());
  else
	return 0;
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoDataDM::ServerModeQueryDataSourceFatalError(TdxServerModeCustomDataSource *Sender, const String AMessage)
{
  String S = "The following error occurred when obtaining data: \"" + AMessage + "\".\n";
  S += "Do you want to reload the data?";
  if (MessageDlg(S, mtError, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
	Sender->Open();
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoDataDM::ServerModeQueryDataSourceInconsistentCache(TdxServerModeCustomDataSource *Sender, const String AMessage, bool &ARecoverCache)
{
  ARecoverCache = MessageDlg("The cache state is inconsistent. Do you want to recover it?", mtError, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes;
  if (!ARecoverCache)
	Sender->Close();
}
//---------------------------------------------------------------------------

