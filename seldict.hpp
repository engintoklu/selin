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


#ifndef SELDICT_HPP
#define SELDICT_HPP

// CUSTOMIZATION:
// here, we decide which data type to use for dictionaries
// you can define the following at the beginning of your program:
// SELIN_USE_MAP (to use std::map<>. this is the default)
// SELIN_USE_UNORDERED_MAP (to use std::unordered_map<>. needs C++11)
// SELIN_USE_TR1_UNORDERED_MAP (to use std::tr1::unordered_map<>. needs TR1)
// SELIN_USE_BOOST_UNORDERED_MAP (to use boost::unordered_map<>)

#if !defined(SELIN_USE_MAP) && !defined(SELIN_USE_UNORDERED_MAP) && !defined(SELIN_USE_TR1_UNORDERED_MAP) && !defined(SELIN_USE_BOOST_UNORDERED_MAP)
#define SELIN_USE_MAP
#endif

#ifdef SELIN_USE_MAP
#include <map>
#endif

#ifdef SELIN_USE_UNORDERED_MAP
#include <unordered_map>
#endif

#ifdef SELIN_USE_TR1_UNORDERED_MAP
#include <tr1/unordered_map>
#endif

#ifdef SELIN_USE_BOOST_UNORDERED_MAP
#include <boost/unordered_map.hpp>
#endif

namespace selin
{
    template<typename Key, typename T>
    class dictionary
    {
    public:
        #ifdef SELIN_USE_MAP
        typedef std::map<Key, T> map;
        #endif

        #ifdef SELIN_USE_UNORDERED_MAP
        typedef std::unordered_map<Key, T> map;
        #endif

        #ifdef SELIN_USE_TR1_UNORDERED_MAP
        typedef std::tr1::unordered_map<Key, T> map;
        #endif

        #ifdef SELIN_USE_BOOST_UNORDERED_MAP
        typedef boost::unordered_map<Key, T> map;
        #endif
    };
}

#endif
