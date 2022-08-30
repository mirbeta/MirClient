{********************************************************************
TODBCLINK design time editors
for Delphi 3.0,4.0,5.0 & C++Builder 3.0,4.0
version 0.4b

written by
  TMS Software
  copyright © 1996-1999
  Email : info@tmssoftware.com
  Web : http://www.tmssoftware.com

The source code is given as is. The author is not responsible
for any possible damage done due to the use of this code.
The component can be freely used in any application. The complete
source code remains property of the author and may not be distributed,
published, given or sold in any form as such. No parts of the source
code can be included in any other component or application without
written authorization of the author.
********************************************************************}
unit odbcde;

interface
{$I TMSDEFS.INC}
uses
  ODBCLink, SysUtils,ODBCCst, Classes,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


type
 TODBCDataSourceProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TODBCDriverProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TODBCTableProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TODBCFieldProperty = class(TIntegerProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                          procedure SetValue(const value:string); override;
                          function GetValue:string; override;
                         end;

 TODBCFieldNameProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;


implementation

function TODBCDataSourceProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCDataSourceProperty.GetValues(Proc: TGetStrProc);
var
 odbc:todbcconnection;
 i:integer;
 strlist:tstrings;
begin
 odbc:=todbcconnection.create(nil);
 strlist:=tstringlist.Create;
 strlist.assign(odbc.datasources);
 odbc.free;
 if strlist.count>0 then
 for i:=0 to strlist.count-1 do
  begin
   proc(strlist.strings[i]);
  end;
 strlist.free;
end;

function TODBCDriverProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCDriverProperty.GetValues(Proc: TGetStrProc);
var
 odbc:todbcconnection;
 i:integer;
 strlist:tstrings;
begin
 odbc:=todbcconnection.create(nil);
 strlist:=tstringlist.Create;
 strlist.assign(odbc.drivers);
 odbc.free;
 if strlist.count>0 then
 for i:=0 to strlist.count-1 do
  begin
   proc(strlist.strings[i]);
  end;
 strlist.free;
end;

{ TODBCTableProperty }

function TODBCTableProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCTableProperty.GetValues(Proc: TGetStrProc);
var
 ODBClink:todbclink;
 ODBC:tODBCConnection;
 strlist:tstrings;
 i:integer;
begin
 ODBClink:=(GetComponent(0) as TODBCLink);

 if ((ODBCLink.Driver <>'') or (ODBCLink.Datasource<>'')) and (ODBCLink.Databasefile <>'') then
  begin
   ODBC:=tODBCConnection.Create(nil);

   if (ODBCLink.Driver<>'') then
    strlist:=ODBC.DriverConnectFile(0,ODBCLink.Driver,ODBCLink.Databasefile)
   else
    strlist:=ODBC.DSNConnectFile(0,ODBCLink.DataSource,ODBCLink.Databasefile);

   //get the tables here

   if strlist.count>0 then
    for i:=0 to strlist.count-1 do
     begin
      proc(strlist.strings[i]);
     end;

   ODBC.Free;
  end;
end;

procedure TODBCFieldProperty.SetValue(const value:string);
begin
 SetOrdValue(0);
 if value='SQL_CHAR' then SetOrdValue(1);
 if value='SQL_NUMERIC' then SetOrdValue(2);
 if value='SQL_DECIMAL' then SetOrdValue(3);
 if value='SQL_INTEGER' then SetOrdValue(4);
 if value='SQL_SMALLINT' then SetOrdValue(5);
 if value='SQL_FLOAT' then SetOrdValue(6);
 if value='SQL_REAL' then SetOrdValue(7);
 if value='SQL_DOUBLE' then SetOrdValue(8);
 if value='SQL_VARCHAR' then SetOrdValue(12);

 if value='SQL_DATE' then SetOrdValue(9);
 if value='SQL_TIME' then SetOrdValue(10);
 if value='SQL_TIMESTAMP' then SetOrdValue(11);
 if value='SQL_LONGVARCHAR' then SetOrdValue(-1);
 if value='SQL_BINARY' then SetOrdValue(-2);
 if value='SQL_VARBINARY' then SetOrdValue(-3);
 if value='SQL_LONGVARBINARY' then SetOrdValue(-4);
 if value='SQL_BIGINT' then SetOrdValue(-5);
 if value='SQL_TINYINT' then SetOrdValue(-6);
 if value='SQL_BIT' then SetOrdValue(-7);
end;


function TODBCFieldProperty.GetValue:string;
begin
 case GetOrdValue of
 1:result:='SQL_CHAR';
 2:result:='SQL_NUMERIC';
 3:result:='SQL_DECIMAL';
 4:result:='SQL_INTEGER';
 5:result:='SQL_SMALLINT';
 6:result:='SQL_FLOAT';
 7:result:='SQL_REAL';
 8:result:='SQL_DOUBLE';
 9:result:='SQL_DATE';
 10:result:='SQL_TIME';
 11:result:='SQL_TIMESTAMP';
 12:result:='SQL_VARCHAR';
 -1:result:='SQL_LONGVARCHAR';
 -2:result:='SQL_BINARY';
 -3:result:='SQL_VARBINARY';
 -4:result:='SQL_LONGVARBINARY';
 -5:result:='SQL_BIGINT';
 -6:result:='SQL_TINYINT';
 -7:result:='SQL_BIT';
 else result:='SQL_UNKNOWN';
 end;
end;


function TODBCFieldProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCFieldProperty.GetValues(Proc: TGetStrProc);
var
 ODBCField:tODBCField;
 ODBCLink:TODBCLink;
 ODBC:tODBCConnection;
 ODBCTypeInfo:TODBCTypeInfo;

begin
 ODBCField:=(GetComponent(0) as TODBCField);

 ODBCLink:=(ODBCField.Collection as TODBCFieldCollection).GetOwner as TODBCLink;

 if ((ODBCLink.Driver<>'') or (ODBCLink.Datasource<>'')) and (ODBCLink.Databasefile <>'') then
  begin
   ODBC:=tODBCConnection.Create(nil);
   if (ODBCLink.Driver<>'') then
    ODBC.DriverConnectFile(0,ODBCLink.Driver,ODBCLink.Databasefile)
   else
    ODBC.DSNConnectFile(0,ODBCLink.Datasource,ODBCLink.Databasefile);


   //get the tables here

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_CHAR);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_CHAR');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_NUMERIC);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_NUMERIC');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_DECIMAL);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_DECIMAL');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_INTEGER);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_INTEGER');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_SMALLINT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_SMALLINT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_FLOAT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_FLOAT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_REAL);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_REAL');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_DOUBLE);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_DOUBLE');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_VARCHAR);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_VARCHAR');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_DATE);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_DATE');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_TIME);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_TIME');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_TIMESTAMP);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_TIMESTAMP');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_LONGVARCHAR);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_LONGVARCHAR');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_BINARY);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_BINARY');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_VARBINARY);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_VARBINARY');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_BIGINT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_BIGINT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_TINYINT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_TINYINT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_BIT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_BIT');

   proc('SQL_UNKNOWN');

   ODBC.Free;

  end;

end;

function TODBCFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCFieldNameProperty.GetValues(Proc:TGetStrProc);
var
 i:integer;
 strlist:tstrings;
 ODBCField:tODBCField;
 ODBCLink:TODBCLink;
 ODBC:tODBCConnection;

begin
 ODBCField:=(GetComponent(0) as TODBCField);

 ODBCLink:=(ODBCField.Collection as TODBCFieldCollection).GetOwner as TODBCLink;

 if ((ODBCLink.Driver<>'') or (ODBCLink.Datasource<>'')) and (ODBCLink.Databasefile <>'') and (ODBCLink.Table<>'') then
  begin
   ODBC:=tODBCConnection.Create(nil);
   if (ODBCLink.Driver<>'') then
    ODBC.DriverConnectFile(0,ODBCLink.Driver,ODBCLink.Databasefile)
   else
    ODBC.DSNConnectFile(0,ODBCLink.Datasource,ODBCLink.Databasefile);

   strlist:=tstringlist.Create;
   strlist.assign(ODBC.Fields(ODBCLink.Table,false));
   ODBC.Free;

   if strlist.count>0 then
   for i:=0 to strlist.count-1 do
    begin
     proc(strlist.strings[i]);
    end;
   strlist.free;
  end;
end;


end.
