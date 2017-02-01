// Copyright (c) 2014-2017, Nihat Engin Toklu < http://github.com/engintoklu >
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
//    1. The origin of this software must not be misrepresented; you must
//    not claim that you wrote the original software. If you use this
//    software in a product, an acknowledgment in the product documentation
//    would be appreciated but is not required.
//
//    2. Altered source versions must be plainly marked as such,
//    and must not be misrepresented as being the original software.
//
//    3. This notice may not be removed or altered from any source
//    distribution.

/*
 * An Automatic Memory Management Library, using Reference Counting
 * Written by Nihat Engin TOKLU
 *
 * Referans Sayimi Yontemiyle Hafiza Yonetim Kutuphanesi
 * Nihat Engin TOKLU
 *
 *
 * Usage:
 *
 * using namespace refcount;
 *
 * ...
 *
 * Ref<MyClass> x(new MyClass());
 * x->value = some_value;
 *
 * Deletion of objects and vectors are automatically done.
 *
 * Beware! Objects with circular references are not collected!
 */

#ifndef REFCOUNT_HPP
#define REFCOUNT_HPP

#include <list>
#include <vector>
#include <iostream>

namespace refcount
{
    int alive_objects_count = 0;

    class Collectable
    {
    public:
        mutable int __refcount;

        Collectable()
        {
            __refcount = 0;
            alive_objects_count++;
            //std::cout << alive_objects_count << ", " << this << " is coming to life" << std::endl;
        }

        virtual ~Collectable()
        {
            alive_objects_count--;
            //std::cout << alive_objects_count << ", " << this << " is dying" << std::endl;
        }

        int get_reference_count() const
        {
            return __refcount;
        }
    };


    void add_address(const Collectable *ptr)
    {
        if(ptr == NULL) return;
        ptr->__refcount++;
    }

    bool remove_address(const Collectable *ptr)
    {
        if(ptr == NULL) return false;

        ptr->__refcount--;

        if (ptr->__refcount == 0) return true;
        return false;
    }

    template<class T>
    class RefBase
    {
    protected:
        T* data;

        void collect()
        {
            if(remove_address(data))
            {
                delete data;
            }
        }

        void initialize()
        {
            data = NULL;
        }

        void initialize(T *target)
        {
            data = target;
            add_address(data);
        }

        void change_ptr(T *target)
        {
            if(target == data) return;

            collect();
            data = target;
            add_address(data);
        }

    public:
        T *get_pointer()             { return data; }
        const T *get_pointer() const { return data; }

    };

    template<class T>
    class Ref: public RefBase<T>
    {
        public:

        Ref()                { RefBase<T>::initialize(); }
        ~Ref()               { RefBase<T>::collect(); }
        Ref(const Ref<T> &r) { RefBase<T>::initialize(r.data); }

        explicit Ref(T *x)   { RefBase<T>::initialize(x); }

        Ref<T> &operator=(const Ref<T>& r)
        {
            RefBase<T>::change_ptr(r.data);
            return *this;
        }

        //Ref<T> &operator=(T *ptr)
        //{
        //    RefBase<T>::change_ptr(ptr);
        //    return *this;
        //}

        bool operator==(const Ref<T> &other) const
        {
            return RefBase<T>::get_pointer() == other.get_pointer();
        }

        bool operator!=(const Ref<T> &other) const
        {
            return RefBase<T>::get_pointer() != other.get_pointer();
        }

        T* operator->()
        {
            return RefBase<T>::data;
        }

        bool is_null() const
        {
            return RefBase<T>::get_pointer() == NULL;
        }

        bool is_not_null() const
        {
            return RefBase<T>::get_pointer() != NULL;
        }

        void nullify()
        {
            RefBase<T>::change_ptr(NULL);
        }

        template<typename T2> Ref<T2> as()
        {
            Ref<T2> result(dynamic_cast<T2*>(RefBase<T>::data));
            return result;
        }

        template<typename T2> Ref<T2> as() const
        {
            Ref<T2> result(dynamic_cast<const T2*>(RefBase<T>::data));
            return result;
        }
    };


    //template<class T_from, class T_to>
    //Ref<T_to> refcast(Ref<T_from> r)
    //{
    //    return dynamic_cast<T_to*>(r.get_pointer());
    //}


    template<class T>
    class CRefBase
    {
    protected:
        const T* data;

        void collect()
        {
            if(remove_address(data))
            {
                delete data;
            }
        }

        void initialize()
        {
            data = NULL;
        }

        void initialize(const T *target)
        {
            data = target;
            add_address(data);
        }

        void change_ptr(const T *target)
        {
            if(target == data) return;

            collect();
            data = target;
            add_address(data);
        }

    public:
        const T *get_pointer() const { return data; }

    };


    template<class T>
    class CRef: public CRefBase<T>
    {
        public:

        CRef()  { CRefBase<T>::initialize(); }
        ~CRef() { CRefBase<T>::collect(); }
        CRef(const CRef<T> &r) { CRefBase<T>::initialize(r.get_pointer()); }

        CRef(const Ref<T> &r)  { CRefBase<T>::initialize(r.get_pointer()); }
        //explicit CRef(const Ref<T> &r)  { CRefBase<T>::initialize(r.get_pointer()); }
        //explicit CRef(Ref<T> &r)        { CRefBase<T>::initialize(r.get_pointer()); }

        explicit CRef(const T* x)       { CRefBase<T>::initialize(x); }

        CRef<T> &operator=(const CRef<T>& r)
        {
            CRefBase<T>::change_ptr(r.data);
            return *this;
        }

        bool operator==(const CRef<T> &other) const
        {
            return CRefBase<T>::get_pointer() == other.get_pointer();
        }

        bool operator!=(const CRef<T> &other) const
        {
            return CRefBase<T>::get_pointer() != other.get_pointer();
        }

        const T* operator->() const
        {
            return CRefBase<T>::data;
        }

        bool is_null() const
        {
            return CRefBase<T>::get_pointer() == NULL;
        }

        bool is_not_null() const
        {
            return CRefBase<T>::get_pointer() != NULL;
        }

        void nullify()
        {
            CRefBase<T>::change_ptr(NULL);
        }

        template<typename T2> CRef<T2> as() const
        {
            CRef<T2> result(dynamic_cast<const T2*>(CRefBase<T>::data));
            return result;
        }
    };

    //template<class T_from, class T_to>
    //CRef<T_to> crefcast(Ref<T_from> r)
    //{
    //    return dynamic_cast<const T_to*>(r.get_pointer());
    //    //return r.as<T_to>();
    //}

    //template <class T_from, class T_to>
    //CRef<T_to> crefcast(CRef<T_from> r)
    //{
    //    return dynamic_cast<const T_to*>(r.get_pointer());
    //    //return r.as<T_to>();
    //}

    template<typename T>
    Ref<T> refnew()
    {
        Ref<T> result(new T());
        return result;
    }

    template<typename T, typename T1>
    Ref<T> refnew(T1 a1)
    {
        Ref<T> result(new T(a1));
        return result;
    }

    template<typename T, typename T1, typename T2>
    Ref<T> refnew(T1 a1, T2 a2)
    {
        Ref<T> result(new T(a1, a2));
        return result;
    }

    template<typename T, typename T1, typename T2, typename T3>
    Ref<T> refnew(T1 a1, T2 a2, T3 a3)
    {
        Ref<T> result(new T(a1, a2, a3));
        return result;
    }

}


#endif
