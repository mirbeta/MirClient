#include "Classes.hpp"
#include "cxCustomData.hpp"

  class TCustomer {
  private:
    int FID;
    String FName;
    String FDescription;
  protected:
    String GetDescription();
    void SetDescription(String Value);
    String GetName();
    void SetName(String Value);
    void SetID(int Value);
  public:
    __property String Description = {read = GetDescription, write = SetDescription};
    __property int ID = {read = FID, write = SetID};
    __property String Name = {read = GetName, write = SetName};
    TCustomer(int AID) {FID = AID; FName = "";};
  };

  class TCustomerList {
  private:
    TList* FList;
    int FNextID;
    void ReleaseAllCustomers();
    void ReleaseCustomer(int AIndex);
    TCustomer* GetCustomer(int AIndex);
    int GetCount();
  public:
    TCustomerList();
    ~TCustomerList();
    void Clear();
    int Add(TCustomer* Customer);
    void Delete(int AIndex);
    void Insert(int AIndex, TCustomer* Customer);
    __property TCustomer* Customers[int Index] = {read = GetCustomer};
    __property int Count = {read = GetCount};
    __property int NextID = {read = FNextID};
  };

  class TCustomerDataSource : public TcxCustomDataSource {
  private:
    TCustomerList* FCustomers;
    bool FModified;
  protected:
    void * __fastcall AppendRecord(void);
    void __fastcall DeleteRecord(void * ARecordHandle);
    int __fastcall GetRecordCount(void);
    Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);
    void * __fastcall InsertRecord(void * ARecordHandle);
    void __fastcall SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue);
  public:
    TCustomerDataSource(TCustomerList* ACustomerList) {FCustomers = ACustomerList;};
    __property bool Modified = {read = FModified};
  };
const int IndexOfID = 0;
const int IndexOfName = 1;
const int IndexOfDescription = 2;

