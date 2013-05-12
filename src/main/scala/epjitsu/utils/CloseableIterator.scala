package epjitsu.utils

import java.io.Closeable

trait CloseableIterator[+A] extends Iterator[A] with Closeable
