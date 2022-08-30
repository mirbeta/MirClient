#include "IniFiles.hpp"
#include "Classes.hpp"
#include "cxCustomData.hpp"
#include "cxGridCustomTableView.hpp"
//---------------------------------------------------------------------------

  class TUserIniFile : public TIniFile {
  private:
    TStringList* FSections;
    bool FModified;
    TNotifyEvent FOnModify;
    String GetSectionName(int AIndex);
    void ReleaseSection(int AIndex);
    void RecreateSections();
    void RecreateValues();
    void SetModified(bool Value);
    void ClearObjects();
  public:
    __fastcall TUserIniFile(const String FileName);
    __fastcall ~TUserIniFile();
    int SectionCount();
    TStrings* InsertSection(int AIndex, String const Section, bool AAppend = false);
    void ChangeSectionName(int AIndex, String const Section);
    void DeleteSection(int AIndex);
    void Clear();
    void LoadValues();
    void SaveValues();
    void SaveAs(String const AFileName);
    void Rename(String const AFileName, bool Reload);
    __property TNotifyEvent OnModify = {read = FOnModify, write = FOnModify};
    __property bool Modified = {read = FModified, write = SetModified};
    __property String SectionNames[int AIndex] = {read = GetSectionName};
    __property TStringList* Sections = {read = FSections};
  };

  class TUserDataSource : public TcxCustomDataSource {
  private:
    TUserIniFile* FIniFile;
  protected:
  	int __fastcall GetRecordCount(void);
  	Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);
  	void __fastcall SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue);
  public:
    TUserDataSource(TUserIniFile* AIniFile) { FIniFile = AIniFile;};
  	void * __fastcall AppendRecord(void);
	  void * __fastcall InsertRecord(void * ARecordHandle);
  	void __fastcall DeleteRecord(void * ARecordHandle);
    __property TUserIniFile* DataIniFile = {read = FIniFile};
  };

  class TUserDetailDataSource : public TcxCustomDataSource {
  private:
    TUserDataSource* FDataSource; // master
  protected:
  	int __fastcall GetRecordCount(void);
  	Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);
  	void __fastcall SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue);
  public:
    TUserDetailDataSource(TUserDataSource* ADataSource) { FDataSource = ADataSource;};
  	void * __fastcall AppendRecord(void);
	  void * __fastcall InsertRecord(void * ARecordHandle);
  	void __fastcall DeleteRecord(void * ARecordHandle);
    int GetMasterRecordIndex();
    __property TUserDataSource* MasterDataSource = {read = FDataSource};
  };
