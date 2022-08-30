program OverbyteIcsHttpsServer;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpsServer1 in 'OverbyteIcsHttpsServer1.pas' {HttpsSrvForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THttpsSrvForm, HttpsSrvForm);
  Application.Run;
end.
