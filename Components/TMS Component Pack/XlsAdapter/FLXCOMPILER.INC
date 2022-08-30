/// Here are the compiler options and defines used by FlexCel. Do not modify this file.

{$DEFINE FLX_VCL}
{$DEFINE FLEXCEL}

{$IFDEF ConditionalExpressions}
  {$if CompilerVersion >= 19} //Delphi 2008 or above
    {$DEFINE DELPHI2008UP}
    {$DEFINE FLX_GENERICS}
    {$DEFINE FLX_HAS_DEPRECATED_COMMENTS}
    {$DEFINE FLX_FPC_OR_UNICODE}
    {$DEFINE USEPNGLIB}
  {$IFEND}
{$ENDIF}

{$IFDEF FPC}
  {$B-,C+,D+,H+,I+,J-,M-,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Z1}

  {$MODE DELPHI}{$H+}
  {$DEFINE FLX_NEEDSVARIANTS}
  {$DEFINE FLX_NEEDSDESIGNINTF}
  {$DEFINE FLX_CANDISABLEWARNINGS}
  {$DEFINE FLX_NEEDSTYPES}
  {$DEFINE WIDEUPPEROK}
  {$DEFINE FLX_HASCUSTOMVARIANTS}

  {$DEFINE FLX_FPC}
  {$DEFINE FLX_FPC_OR_CLX}
  {$DEFINE FLX_FPC_OR_UNICODE}
  {$DEFINE USEPNGLIB}
  {$DEFINE NOWIDESTRINGGRAPHICS} //Try to uncomment this in newer Lazarus versions.
  {$DEFINE NOFORMATSETTINGS}  //Try to uncomment this in newer Lazarus versions.

{$ELSE}
  {$B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

  {$DEFINE FLX_NEEDSJPEG}
  {$DEFINE FLX_SUPPORTSWMF}
  {$DEFINE FLX_WINDOWSCLIPBOARD}

  {$IFDEF CONDITIONALEXPRESSIONS}  //Delphi 6 or newer
    {$DEFINE FLX_NEEDSVARIANTS}
    {$DEFINE FLX_HASCUSTOMVARIANTS}
    {$DEFINE FLX_NEEDSDESIGNINTF}
    {$DEFINE FLX_CANDISABLEWARNINGS}
    {$DEFINE FLX_NEEDSTYPES}

    {$if CompilerVersion < 15}
    {$DEFINE NOFORMATSETTINGS}
    {$DEFINE FLX_NEEDSPOSEX}
    {$IFEND} //Delphi 6

    {$if CompilerVersion >= 18}  //Delphi 2006 or newer, this is fixed there.
    {$DEFINE WIDEUPPEROK}
    {$DEFINE FLX_FIX_PARENTBACKGROUND}
    {$ifend}

  {$ELSE}
    {$DEFINE FLX_EXTRAWINDOWS}
    {$DEFINE FLX_NO_TSIZE}
    {$DEFINE NOFORMATSETTINGS}
    {$DEFINE FLX_NEEDSPOSEX}
    {$DEFINE FLX_NOVARDATEFROMSTRING}
  {$ENDIF}
{$ENDIF}

{$IFDEF ConditionalExpressions}
      {$if CompilerVersion >= 18}
         {$DEFINE BCB2006UP}
      {$ifend}
      {$if CompilerVersion >= 22}
         {$DEFINE DELPHIXEUP}
      {$ifend}
      {$if CompilerVersion >= 24}
         {$DEFINE DELPHIXE3UP}
      {$ifend}
      {$if CompilerVersion >= 25} 
         {$DEFINE DELPHIXE4UP}
      {$ifend}
{$ENDIF}


