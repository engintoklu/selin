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

#ifndef SELSTR_HPP
#define SELSTR_HPP

#include <cstring>
#include <string>
#include <cstdlib>
#include <sstream>
//#include <locale>

// CUSTOMIZATION
// Here we decide whether to use char strings or wchar_t strings
// you can define the following at the beginning of your program:
// SELIN_STRING_USE_CHAR  (to use char strings. this is the default)
// SELIN_STRING_USE_WCHAR_T  (to use wchar_t strings)

#ifndef SELIN_STRING_USE_CHAR
    #ifndef SELIN_STRING_USE_WCHAR_T
        #if defined(_WIN32) || defined(WIN32)
            #define SELIN_STRING_USE_CHAR
        #else
            #define SELIN_STRING_USE_WCHAR_T
        #endif
    #endif
#endif

// CUSTOMIZATION
// if you define SELIN_STRING_USE_BOOST at the beginning,
// uppercasing and lowercasing operations will be done using boost.
// if boost is used, such operations are locale-aware
#ifdef SELIN_STRING_USE_BOOST
#include <boost/algorithm/string.hpp>
#endif

namespace selin
{

    class widestring
    {
    public:
        #ifdef SELIN_STRING_USE_CHAR
        typedef char character;
        #endif

        #ifdef SELIN_STRING_USE_WCHAR_T
        typedef wchar_t character;
        #endif

        typedef character value_type;

    private:
        std::basic_string<character> value;

        void init_from_char(const char *c) throw()
        {
            #ifdef SELIN_STRING_USE_CHAR
            value = c;
            #endif

            #ifdef SELIN_STRING_USE_WCHAR_T
            size_t len;
            wchar_t *data;

            len = strlen(c) + 1;
            data = new wchar_t[len];

            mbstowcs(data, c, len);

            value = data;

            delete []data;
            #endif
        }

    public:
        widestring()
        {
        }

        #ifndef SELIN_STRING_USE_CHAR
        widestring(const char *c) throw()
        {
            init_from_char(c);
        }

        widestring(const std::string &s) throw()
        {
            init_from_char(s.c_str());
        }
        #endif

        widestring(const character *c)
        {
            value = c;
        }

        widestring(const std::basic_string<character> &s)
        {
            value = s;
        }

        widestring(const widestring &s)
        {
            value = s.value;
        }

        character &operator [](size_t i)
        {
            return value[i];
        }

        character operator [](size_t i) const
        {
            return value[i];
        }

        size_t size() const
        {
            return value.size();
        }

        void reserve(size_t n) throw()
        {
            value.reserve(n);
        }

        size_t capacity() const
        {
            return value.capacity();
        }

        widestring substr(size_t a) const throw()
        {
            return value.substr(a);
        }

        widestring substr(size_t a, size_t b) const throw()
        {
            return value.substr(a, b);
        }

        widestring &operator =(const widestring &s)
        {
            value = s.value;
            return *this;
        }

        widestring &operator +=(const widestring &s)
        {
            value += s.value;
            return *this;
        }

        bool operator ==(const widestring &s) const
        {
            return value == s.value;
        }

        bool operator !=(const widestring &s) const
        {
            return value != s.value;
        }

        bool operator >(const widestring &s) const
        {
            return value > s.value;
        }

        bool operator <(const widestring &s) const
        {
            return value < s.value;
        }

        bool operator >=(const widestring &s) const
        {
            return value > s.value;
        }

        bool operator <=(const widestring &s) const
        {
            return value < s.value;
        }

        std::string to_string() const
        {
            #ifdef SELIN_STRING_USE_CHAR
            return value;
            #endif

            #ifdef SELIN_STRING_USE_WCHAR_T
            size_t len;
            char *data;
            std::string result;

            len = value.size() * 4 + 1;
            data = new char[len];

            wcstombs(data, value.c_str(), len);

            result = data;

            delete []data;

            return result;
            #endif
        }

        std::basic_string<character> to_basic_string() const
        {
            return value;
        }

        #ifndef SELIN_STRING_USE_CHAR
        friend bool operator ==(const std::string &a, const widestring &b);
        friend bool operator !=(const std::string &a, const widestring &b);
        friend bool operator >(const std::string &a, const widestring &b);
        friend bool operator <(const std::string &a, const widestring &b);
        friend bool operator >=(const std::string &a, const widestring &b);
        friend bool operator <=(const std::string &a, const widestring &b);
        #endif

        friend bool operator ==(const std::basic_string<character> &a, const widestring &b);
        friend bool operator !=(const std::basic_string<character> &a, const widestring &b);
        friend bool operator > (const std::basic_string<character> &a, const widestring &b);
        friend bool operator < (const std::basic_string<character> &a, const widestring &b);
        friend bool operator >=(const std::basic_string<character> &a, const widestring &b);
        friend bool operator <=(const std::basic_string<character> &a, const widestring &b);

        friend std::ostream& operator<< (std::ostream &out, const widestring &s);
        friend std::istream& operator>> (std::istream &in, widestring &s);
    };

    #ifndef SELIN_STRING_USE_CHAR
    bool operator ==(const std::string &a, const widestring &b) { return b == a; }
    bool operator !=(const std::string &a, const widestring &b) { return b != a; }
    bool operator > (const std::string &a, const widestring &b) { return b < a; }
    bool operator < (const std::string &a, const widestring &b) { return b > a; }
    bool operator >=(const std::string &a, const widestring &b) { return b <= a; }
    bool operator <=(const std::string &a, const widestring &b) { return b >= a; }
    #endif

    bool operator ==(const std::basic_string<widestring::character> &a, const widestring &b) { return a == b.value; }
    bool operator !=(const std::basic_string<widestring::character> &a, const widestring &b) { return a != b.value; }
    bool operator > (const std::basic_string<widestring::character> &a, const widestring &b) { return a > b.value; }
    bool operator < (const std::basic_string<widestring::character> &a, const widestring &b) { return a < b.value; }
    bool operator >=(const std::basic_string<widestring::character> &a, const widestring &b) { return a >= b.value; }
    bool operator <=(const std::basic_string<widestring::character> &a, const widestring &b) { return a <= b.value; }

    std::ostream& operator<< (std::ostream &out, const widestring &s) { return out << s.to_string(); }
    std::istream& operator>> (std::istream &in, widestring &s)
    {
        std::string data;
        in >> data;
        s = data;
        return in;
    }


    #ifdef SELIN_STRING_USE_BOOST
    // proper locale-aware uppercasing and lowercasing by using boost
    widestring uppercase(const widestring &ss)
    {
        std::string s;
        s = ss;
        boost::to_upper(s);
        return s;
    }

    widestring lowercase(const widestring &ss)
    {
        std::string s;
        s = ss;
        boost::to_lower(s);
        return s;
    }
    #else
    // locale-unaware, English-only uppercase and lowercasing
    widestring uppercase(const widestring &ss)
    {
        typedef widestring::character character;
        const int difference = int('a') - int('A');
        widestring s = ss;
        size_t i;
        for (i = 0; i < s.size(); i++)
        {
            if (s[i] >= character('a') && s[i] <= character('z'))
            {
                s[i] -= character(difference);
            }
        }
        return s;
    }

    widestring lowercase(const widestring &ss)
    {
        typedef widestring::character character;
        const int difference = int('a') - int('A');
        widestring s = ss;
        size_t i;
        for (i = 0; i < s.size(); i++)
        {
            if (s[i] >= character('A') && s[i] <= character('Z'))
            {
                s[i] += character(difference);
            }
        }
        return s;
    }
    #endif


    class StringBuilder
    {
    private:
        std::list<widestring> data;
        size_t total_length;

        void clone_from(const StringBuilder &other)
        {
            data = other.data;
            total_length = other.total_length;
        }

    public:
        StringBuilder()
        {
            total_length = 0;
        }

        StringBuilder(const StringBuilder &other)
        {
            clone_from(other);
        }

        StringBuilder &operator =(const StringBuilder &other)
        {
            clone_from(other);
            return *this;
        }

        void add(const widestring &s)
        {
            data.push_back(s);
            total_length += s.size();
        }

        widestring str() const
        {
            widestring result;

            result.reserve(total_length + 1);

            for (std::list<widestring>::const_iterator it = data.begin();
                 it != data.end();
                 it++)
            {
                result += *it;
            }

            return result;
        }

    };

    template <typename T>
    T strreplace(const T &bigstr, const T &what, const T &with_what)
    {
        typename T::value_type c[2];
        c[1] = (typename T::value_type)(0);

        size_t what_size;
        what_size = what.size();

        T result;
        result.reserve(bigstr.size() * 2);

        size_t i = 0;
        while (i < bigstr.size())
        {
            size_t room_left;
            room_left = bigstr.size() - i;

            if (room_left >= what_size
                && bigstr.substr(i, what_size) == what)
            {
                result += with_what;
                i += what_size;
            }
            else
            {
                c[0] = bigstr[i];
                result += c;
                i += 1;
            }
        }

        return result;
    }
}

#endif
